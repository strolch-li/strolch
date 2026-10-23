# Transaction-local flush and committed root publication

## Contract

Transient (including empty) and cached realm transactions keep a sparse overlay of changed Resources, Orders and Activities. Committed roots remain immutable. A writable transaction still receives clones; searches merge its overlay with committed roots. Pending deletions disappear from its point lookups and searches. No whole-type index is copied and unrelated writers are not rejected by a realm revision.

`flush()` runs commands to completion and clears fetched-object caches. Model changes remain local; it does not call model DAOs or commit persistence. Commands executed by an earlier flush are not executed again at commit. Rollback discards their model changes; custom command `undo()` still runs. `reset()` also discards flushed model changes, keeping element locks.

At commit:

1. Run and validate all remaining customer/model commands, including commands they enqueue. Acquire changed-root locks before preparing persistence.
2. Prepare root buckets, immutable root values, deletion markers, and history operations. Acquire the persistence gate (shared for ordinary writes, exclusive for administrative bulk clears).
3. Queue and execute DAO operations and audit writes; commit persistence.
4. Publish only the prepared clear/insert/replace/remove operations under a short in-memory barrier. This stage contains no customer callbacks, privilege checks, version calculations, or SQL.
5. Notify observers and complete transaction reporting. Element locks remain held until transaction close.

Point reads check the announced affected roots and wait only when their own root is being published; unaffected immutable roots remain readable. Searches and counts use the publication barrier to capture a whole result. Separate reads made at different times may observe different commits. A long-running transaction has no automatic snapshot, repeatable reads, predicate locks or serializable isolation. Cached fetched clones retain their values until refreshed or cleared.

## Business decisions and locking

Use `readLock()` before deciding from mutable business state, and use the refreshed object it returns. Hold every required dependency lock until close; acquire multiple locks in a consistent order. Ordinary readers continue to read immutable committed data. Competing writers and explicit locking readers follow the existing element-lock retry/timeout policy.

```java
try (StrolchTransaction tx = realm.openTx(certificate, MyService.class, false).rollbackOnFailure()) {
    Resource stock = tx.readLock(tx.getResourceBy("ProductStock", stockId, true));
    // Decide from the refreshed stock, then update the private clone and add the order.
    tx.update(stock);
    tx.flush(); // Only this transaction can see its changes.
    tx.commitOnClose();
}
```

Absence or a search predicate cannot be protected by locking only the roots returned by a search. Business logic must use an existing shared coordination root when protecting these invariants. Administrative bulk clears must run with application traffic quiesced: their persistence gate prevents commit interleaving, but they do not acquire every individual root lock. Strolch does not automatically prevent write skew or phantoms.

## History and persistence failure

History versions are finalized during persistence preparation. Updates and deletions reuse the locked current version; they do not add a latest-version SQL lookup. New identifiers may need that lookup during preparation to account for deleted history. In particular, a freshly created root reusing an identifier with deleted history may have a provisional version after local flush; commit assigns the next persisted version without a SQL lookup during flush. Explicit historical reads and custom commands that themselves access persistence or external systems retain their behavior; local flush does not make those effects transactional.

Pre-commit validation or SQL-write failure rolls back persistence and leaves committed model roots unchanged. An acknowledged database commit is never followed by compensating model undo or database rollback because an observer or publication step failed. A confirmed commit followed by failure throws `StrolchPostCommitException` and retains `COMMITTED` state.

If a commit call fails without an acknowledged outcome, the transaction reports `UNKNOWN` through `StrolchCommitOutcomeUnknownException`. The realm stops serving transaction model reads/writes until recovery. A publication failure likewise fences the realm. This conservative policy may require recovery even when the database ultimately rolled back. Restart/recreate the realm from committed persistence; do not clear the fence and serve stale memory, or blindly retry a non-idempotent business operation. A connection cleanup error after an acknowledged PostgreSQL commit is logged and publication still proceeds.

Hash-map publication still relies on a functioning JVM; allocation failure, hardware failure and process termination cannot be made impossible. A crashed cached realm reloads database state on restart. This change does not implement cluster replication, distributed locks, automatic outcome reconciliation, or failover.

## Compatibility and verification

The contract covers transaction access through `StrolchTransaction` and its element maps. Direct writes to internal realm maps are for startup/loading and bypass it. EclipseStore retains its existing storage implementation and is not covered by the new overlay guarantee. Audits, logs and arbitrary external side effects are not included in root-batch publication.

Regression tests include transient and PostgreSQL modes with history off/on, local visibility, rollback, command failures, independent writers, all three root kinds, bulk clears, reset, history undo/revert, SQL commit ordering, no model SQL during flush, lost commit responses, acknowledged-commit cleanup errors, and fail-closed publication errors. A separate barrier test forces a half-published batch and verifies that a query retries after publication.

Run against a disposable PostgreSQL database named `strolch_acid_test`, user `strolch_acid_test`, password `test`, default port 55432:

```sh
mvn -pl strolch-service -am test -Dtest=TransactionPublicationTest \
  -Dsurefire.failIfNoSpecifiedTests=false -Dtransaction.postgres=true
```

Without `-Dtransaction.postgres=true`, this test runs only transient cases. Set `-Dtransaction.dbPort=...` to use another isolated port. The full existing integration suite also needs its own disposable test databases; `-Ddb.hostOverride=localhost:55432` redirects its configured URLs. Do not run schema-dropping integration tests against production databases.

Performance methodology and measured comparisons are in [the benchmark notes](../strolch-jmh-benchmark/TRANSACTION-PUBLICATION.md).
