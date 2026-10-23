# Transient transaction isolation

Transient realms now read from a realm snapshot and publish model changes atomically at commit.
`flush()` executes model commands against transaction-private maps: another transaction cannot read
those changes, and rollback discards them. Resources, Orders and Activities share one snapshot revision,
so a transaction keeps a consistent view across direct lookups and searches while another commits.
Transactions starting after a successful commit see the new model.

## Conflicts and locking

A model-writing transaction validates that its starting realm revision is still current, both before
command execution and at publication. If another writer has committed, this transaction aborts with
`StrolchTransactionConflictException` as the cause of the normal `StrolchTransactionException`.
This deliberately conservative check rejects stale writers even when they changed different roots;
it prevents write skew without introducing predicate-lock infrastructure in this change.
Read-only snapshot transactions can continue across intervening commits.

Existing element locks still exclude conflicting owners. `readLock()` refreshes the locked root from
the current committed realm, leaving any retained immutable copy unchanged. If this refresh mixes a newer
revision with an older snapshot, a subsequent commit or normal read-only completion rejects the transaction.
Explicit rollback remains permitted. Freshness does not silently discard earlier reads or promote the
whole transaction to a newer snapshot.

Applications may retry the **complete transaction** after checking its exception cause. There is no automatic
retry: customer commands may have external effects. Never treat an arbitrary transaction exception as a
retryable conflict. This change does not make emails, device commands or other external effects rollback-safe.

## Implementation and cost

A transient transaction takes shallow copies of the three maps of root types under the realm monitor.
Immutable roots and their per-type indexes are shared initially. The first mutation or lock refresh of a
type copies that type's index; root model graphs are not deep-copied merely to open a transaction.
Commit validates and publishes all three maps under the same monitor. Published maps are separate from the
transaction's mutable maps, so undo after an error cannot modify the already published model.

Opening a transaction costs O(number of root types); copying a modified type costs O(roots in that type).
Long-running snapshots retain old roots/indexes until released. The conservative realm revision can cause
unnecessary aborts under concurrent unrelated writes. This is a correctness-first local implementation,
not a fine-grained distributed concurrency protocol or a performance qualification.

Use transaction map accessors (`tx.getResourceMap()` etc.) for transactional work. Realm-level maps are
internal committed-state views; retaining one across publications is not a live transaction view. Direct
mutation of realm internals bypasses this transaction protocol and is unsupported.

Empty and EclipseStore realms reuse the original `TransientTransaction` implementation and retain their
existing behavior. PostgreSQL cached realms also retain their current semantics; this change does not fix
the PostgreSQL/history failures found in PR #40. Audit-trail storage, observer delivery and post-commit
exception/outcome reporting retain their existing behavior and are not qualified by these model-isolation tests.
Transient mode remains non-durable. These tests do not certify universal ACID or strict serializability.

## Regression tests

The new `TransientTransactionIsolationTest` runs normally, without an opt-in flag or database:

```bash
mvn -pl strolch-service -am test \
  -Dtest=li.strolch.transaction.TransientTransactionIsolationTest \
  -Dsurefire.failIfNoSpecifiedTests=false
```

The sixteen cases cover staged/flushed privacy, rollback, failure after flush, post-commit freshness,
fractured reads, write skew with an explicit conflict outcome, repeated searches, own insertion, lock
exclusion/release, lock refresh, mixed-revision read rejection, mixed-root publication during command
execution, stale flushed writer rejection, bulk removal and delete/recreate snapshot retention.
Unlike the diagnostic baseline, write skew counts only a specifically identified conflict as a safe abort;
unexpected exceptions and timeouts still fail. Both writers committing still fails.

Validation uses Temurin JDK 25.0.4+7 and Maven 3.9.16. The four original transient counterexamples are also
run against the unchanged develop production implementation to confirm regression sensitivity. PR #40
remains a separate diagnostic PR; this PR is based directly on develop and includes none of its database
fixtures or clustering specifications.

### Verified on 2026-10-23

* Negative control at develop `39d4416f0`: all four original counterexamples fail (four assertions,
  zero infrastructure errors). Only the test class and inert conflict-exception API were copied there;
  no production transaction/map changes were applied.
* New transient regression suite: 16 tests, zero failures/errors/skips.
* Clean reactor compile plus the new suite and five selected agent classes: 40 tests passed, zero skips.
* Full `mvn -pl strolch-agent -am test`: 649 discovered tests, zero failures/errors, 14 existing skips
  across utils, privilege, model and agent. This includes Empty/EclipseStore realm startup coverage.

The selected clean command uses fully qualified class names to avoid accidentally selecting the unrelated
PostgreSQL module's identically named `RealmTest`:

```bash
mvn -pl strolch-service -am clean test \
  -Dtest=li.strolch.transaction.TransientTransactionIsolationTest,li.strolch.search.StrolchSearchTest,li.strolch.agent.RealmTest,li.strolch.agent.ComponentContainerTest,li.strolch.agent.ObserverHandlerTests,li.strolch.agent.ParallelTests \
  -Dsurefire.failIfNoSpecifiedTests=false
```

The complete PostgreSQL/service integration suites were not run for this transient-only fix. The isolated
regressions and the existing agent reactor require no PostgreSQL instance.
