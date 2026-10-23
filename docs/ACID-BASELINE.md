# ACID and strict-serializability baseline

This is a **test-only diagnostic baseline**, added before changing transaction or clustering code.
It asserts the desired behaviour rather than encoding suspected anomalies as acceptable results.
It is opt-in because existing implementations may fail these stronger requirements.
Without `-Dacid.baseline=true`, JUnit reports the suite as skipped. A skipped suite is not validation.

## Run

Use JDK 25 and Maven. From the repository root, without PostgreSQL:

```bash
mvn -pl strolch-service -am test \
  -Dtest=TransactionIsolationBaselineTest \
  -Dsurefire.failIfNoSpecifiedTests=false \
  -Dacid.baseline=true -Dacid.transientOnly=true
```

For the complete initial matrix, create a **dedicated disposable** PostgreSQL database first:

```sql
CREATE USER strolch_acid_test WITH PASSWORD 'test';
CREATE DATABASE strolch_acid_test OWNER strolch_acid_test;
```

These are local test credentials only. The fixture connects to localhost, uses four connections and permits
initial schema creation, but not schema drop or migration. It never drops an existing database. Random test
types avoid collisions; data/history remains in this disposable database for diagnosis and accumulates
across runs. Recreate it between baseline campaigns. Do not run campaigns concurrently against the same DB.

```bash
mvn -pl strolch-service -am test \
  -Dtest=TransactionIsolationBaselineTest \
  -Dsurefire.failIfNoSpecifiedTests=false \
  -Dacid.baseline=true
```

Use the existing `-Ddb.hostOverride=db` setting when the database is reached as `db` from a CI container.
The database/user must exist on that server. This suite does not depend on the other service-test databases.

Expect **8 cases in transient mode**, or **24 parameterized cases** for transient, PostgreSQL without history
and PostgreSQL with history. Inspect `strolch-service/target/surefire-reports` and check for skipped/error cases;
an upstream build failure or zero discovered tests is not a passing baseline. Missing DB access is an error,
not an automatic skip.

## What the cases establish

| Case | Required observation |
|---|---|
| Staged update | Only the writing transaction sees the new value; rollback leaves the committed value. |
| Flushed update | `flush()` does not publish uncommitted values; rollback restores the old state. |
| Real-time freshness | A transaction begun after another completed commit observes its change. |
| Fractured read | A completed transaction cannot read a=1, b=2 when another atomically changes (a,b) from (1,1) to (2,2). |
| Write skew | Two transactions reading each other's enabled flag cannot both commit disabling their own flag, even with disjoint write locks. |
| Phantom | A completed read transaction cannot observe a different predicate set after an intervening insert. |
| Own insert | A search includes the transaction's staged insertion. |
| Lock lifecycle | A competing transaction cannot acquire the same lock until its owner closes. |

Coordination uses latches and future completion, not sleep-based ordering. Futures and latches have bounded
waits, and executor cleanup interrupts/releases waiting work. The four-connection PostgreSQL pool is
intentional: a pool of one can serialize transaction admission and conceal anomalies.

The fractured-read and phantom assertions run **after the reader closes successfully**. The write-skew
invariant is checked only after both writers return successfully. Aborts are not silently accepted:
exceptions remain diagnostic test errors, with their stack traces available for classification.

## Interpreting failures

An assertion describing an impossible **completed** history is evidence of a violated requirement.
An infrastructure failure, unexpected abort or timeout is not by itself evidence of nonserializability.
Some schedules deliberately require an intervening writer to complete while a reader is open. A future
implementation that protects reads with locks may block/abort that schedule correctly. Classify such
results and extend the harness to record outcomes instead of weakening the safety requirement.

The private-read tests likewise currently expect a concurrent old-value read to complete. A locking reader
that waits for rollback could also preserve correctness; its timeout must be reported as inconclusive for
safety. This suite targets the current nonblocking-read implementation as the first diagnostic baseline.

Do not catch arbitrary exceptions and count them as successful conflict detection. In particular, a lost
commit response is an unknown outcome, not proof of rollback. Do not mark known failing assertions as
ignored simply to make the baseline green.

## Validation status

Authored against `39d4416f0bb4f6b340f151aa9fc1e5f2d2e13bfe`.
**Not compiled or executed in the authoring workspace:** only Java 17 is installed and Maven is absent,
whereas the repository targets JDK 25. Source/API inspection and whitespace checks are not a test run.
No production code or dependency versions are changed by this baseline.

## Gate before claiming ACID qualification

This first suite is not a certificate or a complete proof. Passing it only establishes its tested cases.
The qualification report must identify the exact commit, persistence settings, isolation contract, OS/JVM,
workloads, fault model, executed histories and results. Further required work:

1. Add a serial reference model/history checker for committed transactions, real-time edges, reads, writes,
   predicate results and unknown outcomes; test the checker against known legal and illegal histories.
2. Cover Resources, Orders and Activities together, repeated reads, missing-object dependencies,
   delete/recreate, local update/delete overlays, release/reacquire and interrupted lock acquisition.
3. Test atomic publication while commit is in progress, injected command/DAO/commit failures, partial batch
   failure, observer exceptions after commit and rollback after earlier mutations.
4. Verify application invariants with concurrent workloads, and realm isolation for identical locators.
   ACID consistency preserves correctly specified invariants; it cannot infer arbitrary business rules.
5. Test PostgreSQL durability with a separate child JVM: acknowledge a commit, kill the application,
   restart/load and verify state. Add DB crash/restart and the intended synchronous-replication failover
   configuration. A graceful close/reopen test alone does not qualify crash durability.
6. After clustering exists, run the same model against several workers/data nodes, adding network
   partitions, node crashes, stale-owner writes, duplicate requests, lost acknowledgements, replay,
   cache races, rebalance, quorum loss and rolling worker versions.
7. Separately evaluate liveness: deadlocks, bounded recovery, resource exhaustion and availability.

Until these gates are satisfied, report **observed conformance and known counterexamples**, not certified ACID.
