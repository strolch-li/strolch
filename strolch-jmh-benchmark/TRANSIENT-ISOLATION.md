# Transient isolation performance qualification

This benchmark compares the production implementation at `develop` with the transient-isolation PR.
It uses the same compiled JMH benchmark class, fixture, external dependencies and JVM settings for both.
Only the reactor production class directories change. Final-state validation runs in shared teardown,
after all workers stop and before the runtime closes. It never uses PostgreSQL or customer data.

**The existing `PerformanceTransientTest` fixture selects EMPTY mode. It cannot measure this PR's
TransientRealm snapshots.** The new fixture uses TRANSIENT and setup asserts the realm mode and root count.

## Workloads and measurements

* Point reads and single-root updates at 1,000, 100,000 and 1,000,000 roots in one type.
* Eight concurrent writers, each with a distinct root: there is deliberately no same-root lock contention.
* Eight workers doing nine completed reads per write attempt, full type searches, and 100-type datasets with the same total
  root count. A separate write case adds 10,000 `Blackhole.consumeCPU` tokens inside the transaction to
  represent customer work; tokens are not a calibrated time unit or a real EAI workload.
* Sampled point-read latency and successful-update latency. The latter includes up to 1,000 full retries
  of specifically recognized conflicts. A lock timeout or exhausted retry bound fails the run. This
  synthetic retry loop is not advice to retry customer code with external effects.
* Attempt throughput, completed reads, committed writes, conflicts and lock timeouts are separate JMH
  metrics. Use **commits/second** for writes, never rejected attempts/second. The summary reports rejection
  as a fraction of write attempts, excluding successful reads.
* JMH's GC profiler records allocation per invocation and GC count/time. For pure writes, the summary
  also computes allocated bytes per successful commit, including wasted conflicting attempts.

Each resource has an ID, name and three parameters (revision, description and SKU). Updates clone and
increment the revision through ordinary `readLock`/`update`/`commitOnClose`. Data is loaded in batches
outside measurement. Setup/login and teardown are excluded. At trial teardown every worker verifies that
its stored revision equals the number of acknowledged successful commits, including warmup operations.
Unknown exceptions fail the benchmark rather than being counted as safe aborts.

All workers share one realm. Separate JVM forks isolate trials. Baseline/candidate ordering alternates
by case; the implementations never run concurrently. Both use JDK 25, G1, a fixed 2 GiB heap and eight
active JVM processors. Framework logging is disabled for both, including expected conflict stack traces,
so exception logging does not artificially dominate the comparison. Exception allocation remains included.
Actual applications that log every conflict can pay additional costs.

## Customer-shaped dataset

`--customer` uses the supplied per-type counts verbatim: 143,211 Resources across 26 types and
146,982 Orders across five types. Because only "a few Activities" was specified, the fixture explicitly
adds ten synthetic Activities in one type, for 290,203 total roots. The doubled case doubles every listed
count and the placeholder Activities, giving 580,406 roots. It does not assume a particular second
customer's distribution. Contents and IDs remain synthetic; no customer records are used.

The customer matrix targets Product (6,802 roots), Person (62,905), PickingItem (45,264) and FromStock
orders (112,787), with single/eight-writer cases, mixed reads/writes, point reads, a doubled FromStock
case and successful-write latency. Eight workers is a benchmark parameter, not a supplied production
concurrency figure. Real transaction durations and read/write mix remain unknown.

## Reproduce

Use two clean worktrees and JDK 25. For example, from the PR checkout:

```bash
git worktree add --detach /tmp/strolch-perf-develop 39d4416f0
# Run in EACH checkout, compiling its own production classes:
mvn -pl strolch-jmh-benchmark -am clean test -DskipTests

# Run in the PR checkout; li.strolch JARs must not hide the checkout's compiled classes:
mvn -pl strolch-jmh-benchmark dependency:build-classpath \
  -DexcludeGroupIds=li.strolch -Dmdep.outputFile=/tmp/strolch-external-cp.txt

python3 strolch-jmh-benchmark/scripts/compare-transient.py \
  --baseline /tmp/strolch-perf-develop --candidate "$PWD" \
  --external-classpath /tmp/strolch-external-cp.txt \
  --output /tmp/strolch-performance --java "$JAVA_HOME/bin/java" --quick
python3 strolch-jmh-benchmark/scripts/summarize-transient.py /tmp/strolch-performance
```

Add `--customer` with a separate output directory for the customer-shaped matrix. Both campaigns use
identical production revisions. The original uniform campaign used the first harness version; the customer
extension adds dataset/root-kind parameters. Each result directory includes its exact `harness-source.java.txt`
and compiled hash. To replay an earlier harness exactly, restore that source into the benchmark source path
and recompile before running the corresponding matrix; do not compare source hashes across harness versions.

The comparison script needs permission for JMH's local control socket and child JVMs. It validates that
reactor class directories exist and rejects an external classpath containing li.strolch JARs. Each fork
prints the code location of the loaded realm implementation for verification.

`--smoke` is only a short harness check. `--quick` uses two forks, two one-second warmups and three
one-second measurements per fork. The default (without either flag) uses three forks, five two-second
warmups and five two-second measurements. Use `--cases write-concurrent latency-successful-write` to select
cases. The driver records exact commands, revisions, working-tree state, compiled harness hash, CPU,
JVM, initial load/memory, timestamps, raw per-iteration JMH results and sample percentiles. The summary
CSV can be regenerated directly from those raw JSON files. Published case JSON/logs are losslessly gzip-compressed
to keep the PR review small; the summarizer reads either form. Add `--compress` to the summarizer command
to archive raw evidence after a run. The environment record, source snapshot and summary remain plain text.

The benchmark module continues to skip its old integration benchmark suite by default. These runs use
JMH directly and do not execute the old suite, which includes database-dependent benchmarks.

## Interpretation and merge gate

Read [the measured comparison](../docs/TransientTransactionPerformance.md) before considering the PR for
merge. Results are a synthetic framework comparison, not customer-site capacity predictions. The tests
are closed-loop and do not include network queues, coordinated-omission correction, real payload graphs,
external integrations, security diversity, long snapshot retention or a soak test. Short runs and a shared
laptop environment make small differences unreliable; inspect confidence intervals and per-fork data.

Correctness improvements do not justify arbitrary throughput loss, tail-latency growth or unnecessary
aborts. Production acceptance needs representative root/type counts, concurrency, transaction duration,
read/write ratio and an agreed regression budget. Run longer repeated comparisons on a controlled host,
then representative service/EAI load and soak tests with real invariants and bounded retries. This PR
must remain draft if the supplied measurements show substantial regressions.
