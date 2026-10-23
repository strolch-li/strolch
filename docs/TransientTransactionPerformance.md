# Transient isolation performance assessment — 2026-10-23

**Merge decision: reject the current implementation on performance grounds; PR #41 remains draft.**
The tests establish useful isolation regressions, but the realm snapshots in this implementation impose
unacceptable write amplification and false conflicts for independent writers. Correctness tests alone
were not sufficient to present this change as a production-ready replacement.

This assessment compares `develop` at `39d4416f0bb4f6b340f151aa9fc1e5f2d2e13bfe` with the
production changes in PR #41 at `a02f110c3e0d2656b1dd8168cb8b7ca1c66bf828`. This extension adds
benchmarks and documentation only; it does not attempt to hide or optimize away the measured regression.
The same compiled benchmark harness runs against each checkout's production classes. The historical
isolation counterexamples target the requested stronger contract; they are not evidence that deployed
customer applications have been unreliable for the past ten years.

## Environment and scope

AMD Ryzen 7 8845HS (8 cores / 16 hardware threads), Linux x86_64, Temurin 25.0.4+7, JMH 1.37,
G1, `-Xms2g -Xmx2g`, `-XX:ActiveProcessorCount=8`. Two independent JVM forks per side, two one-second
warmups and three one-second measurement iterations per fork. Runs alternate baseline/candidate ordering
by case, with no overlap between benchmark JVMs. Model creation and authentication are outside timing;
logging is disabled identically on both sides. Every trial checks acknowledged commits against stored revisions.

This was a shared development laptop, with initial load averages approximately 6/5.5/5.6 and about 8 GiB
available RAM for the uniform campaign (customer campaign initial load approximately 5.5/7.2/6.8),
not a dedicated production performance host. Confidence intervals and raw per-fork values
are retained. Small differences and read-throughput estimates need longer controlled runs; the very large
write regression appears in both forks and cannot be explained by a small amount of environmental noise.

The uniform campaign uses synthetic Resources with three parameters; the customer campaign also loads
Orders and placeholder Activities. Both use a real TRANSIENT realm. Concurrent workers
use disjoint roots. This is not an end-to-end EAI/service load test, and absolute capacity numbers must not
be extrapolated to customer sites. Existing benchmark fixtures using EMPTY mode are not used.

## Customer-shaped measurements

The supplied type counts total **143,211 Resources and 146,982 Orders**. The fixture keeps every listed
type/count, adds ten explicitly synthetic placeholder Activities (the exact count was unspecified), and
therefore loads 290,203 roots. A doubled case loads 580,406 roots. All content and IDs are synthetic.
Products have 6,802 roots, Persons 62,905, PickingItems 45,264 and FromStock orders 112,787.
The chosen eight-worker concurrency and 9:1 mixed workload are assumptions for sensitivity testing,
not measured customer traffic. Object graphs, business logic and EAI effects are not reproduced.

Rates count committed writes (or completed reads plus commits for mixed workloads). All concurrent
writers use distinct roots. Allocation is bytes per attempted transaction, including rejected attempts.

| Customer-shaped workload | Develop successful ops/s | PR successful ops/s | PR / develop | PR writes rejected | Bytes/attempt develop → PR |
|---|---:|---:|---:|---:|---:|
| customer-product-single | 215,193 | 8,443 | 0.03923× | 0.0% | 11,113 → 299,591 |
| customer-product-eight | 426,386 | 3,069 | 0.00720× | 47.0% | 10,973 → 299,610 |
| customer-person-eight | 356,093 | 68 | 0.00019× | 79.9% | 10,981 → 2,554,834 |
| customer-picking-eight | 355,703 | 188 | 0.00053× | 74.2% | 10,978 → 1,727,836 |
| customer-fromstock-single | 210,309 | 156 | 0.00074× | 0.0% | 11,141 → 4,676,209 |
| customer-fromstock-eight | 348,856 | 24 | 0.00007× | 85.8% | 11,053 → 4,675,736 |
| customer-mixed-fromstock | 938,373 | 1,510 | 0.00161× | 74.6% | 2,640 → 472,545 |
| customer-read-fromstock | 1,040,985 | 431,503 | 0.41451× | 0.0% | 1,688 → 4,300 |
| customer-double-fromstock | 340,119 | 12 | 0.00004× | 85.8% | 11,017 → 9,333,742 |

| Successful FromStock write latency, µs | Develop | PR |
|---|---:|---:|
| p50 | 5.58 | 166,199.30 |
| p95 | 17.66 | 672,346.93 |
| p99 | 44.80 | 1,217,103.13 |

This latency includes full synthetic retries of recognized conflicts, so it measures completion rather
than rejection latency. The short-run tail estimate is exploratory; the raw sample counts/distribution
are retained. These data fail the performance gate even without assuming an exact production workload.

The doubled case doubles FromStock to 225,574 roots as well as all other supplied counts. It is a
sensitivity case, not a measured distribution from the second customer. Read/write proportions and
eight-thread concurrency remain explicit benchmark assumptions.

## Paired measurements

Rates below count successful operations: committed writes for write-only cases, completed reads for
read/search cases, and their sum for the mixed case. “Rejected” applies only to write attempts.
Allocation is bytes per attempt, including conflicts. Values are rounded means of six measurement iterations.

| Workload | Develop successful ops/s | PR successful ops/s | PR / develop | PR writes rejected | Bytes/attempt develop → PR |
|---|---:|---:|---:|---:|---:|
| read-small | 1,132,348 | 947,122 | 0.83642× | 0.0% | 1,656 → 2,264 |
| read-large | 1,115,316 | 919,828 | 0.82472× | 0.0% | 1,656 → 2,264 |
| read-million | 1,033,859 | 793,584 | 0.76759× | 0.0% | 1,656 → 2,264 |
| write-small | 219,880 | 53,977 | 0.24548× | 0.0% | 10,941 → 52,836 |
| write-large | 207,120 | 185 | 0.00089× | 0.0% | 10,849 → 4,263,179 |
| write-million | 177,574 | 16 | 0.00009× | 0.0% | 10,822 → 40,405,639 |
| write-concurrent | 363,100 | 32 | 0.00009× | 83.6% | 10,967 → 4,264,285 |
| mixed-concurrent | 838,171 | 1,730 | 0.00206× | 75.0% | 2,588 → 429,247 |
| search-large | 206 | 238 | 1.15429× | 0.0% | 803,147 → 803,733 |
| read-many-types | 1,157,095 | 189,266 | 0.16357× | 0.0% | 1,656 → 9,432 |
| write-many-types | 353,165 | 24,566 | 0.06956× | 8.4% | 10,969 → 67,088 |
| write-service-work | 216,830 | 26 | 0.00012× | 85.7% | 10,882 → 4,264,721 |

Names encode small=1,000 roots, large=100,000, million=1,000,000. Concurrent/mixed/service-work and
latency cases use 100,000 roots and eight workers; many-types uses 100 types, all other cases one.
Read-many-types uses one worker and write-many-types uses eight. Other throughput cases use one worker.
Service-work adds 10,000 CPU tokens; other cases add none.

| Sampled latency, µs | Develop p50 | PR p50 | Develop p95 | PR p95 | Develop p99 | PR p99 |
|---|---:|---:|---:|---:|---:|---:|
| latency-read | 0.93 | 1.12 | 3.22 | 4.83 | 81.66 | 179.97 |
| latency-successful-write | 5.61 | 201,981.95 | 19.94 | 717,592.99 | 45.76 | 1,308,434.10 |

The successful-write latency includes the complete synthetic retry loop. Its tail therefore captures
the cost of obtaining a commit, rather than treating a rejected attempt as completed work.
Sample counts and distributions are in the raw JSON; quick-run tail estimates are exploratory.

For the single-writer 100,000-root case, JMH’s 99.9% confidence interval was approximately
192,104–222,136 ops/s on develop and 145–225 ops/s on the PR. The observed per-iteration ranges
were about 201,000–215,000 versus 166–200 ops/s. Both forks reproduce the severe regression.
Read and search throughput confidence intervals overlap in several cases; no small read/search
speedup or slowdown is claimed as established by this quick campaign. Allocation differences are much more stable.

At eight independent writers, allocated bytes per successful commit were approximately
10,963 on develop and 23,940,804 on the PR, including wasted attempts.
GC time/count and allocation-rate metrics are retained in every raw JMH result.

## Why this implementation fails the performance gate

1. The first write or lock refresh copies the entire root-type index. A small Product change therefore
   scales with the number of Products in that type. Allocation grows from roughly 11 KB per attempt on
   develop to about 4.3 MB at 100,000 roots and 40 MB at one million roots. Root graphs are not all copied,
   but index copying alone is enough to collapse write throughput.
2. Validation compares one revision for the whole realm. Eight threads updating eight different roots
   still invalidate each other's snapshots. Attempt throughput overstates useful work because most
   candidate attempts can be rejected; the report uses committed writes as the primary write measure.
3. Opening each transaction copies the map of types. Point-read allocation rises, especially with many
   types. Long snapshots can additionally retain old objects/indexes, which this short benchmark does not
   quantify as a retention or leak test.

The many-type case reduces the number of roots copied per write, but does not remove realm-wide false
conflicts. Adding ordinary customer work also exposes wasted work before rejection. A successful-write
latency benchmark explicitly includes retry time; a fast rejection is not reported as a fast successful
business operation.

## What must change before this can be considered for merge

The current approach should not replace the established production engine. A revised implementation needs
transaction-local changes without O(roots-in-type) copying for a single-root update, and dependency
validation fine enough for independent writers to commit independently. Search/absence dependencies and
atomic multi-root publication still need protection; optimizing by removing those guarantees would defeat
the purpose of the fix. This is design work, not a claim that a particular replacement has been validated.

Keep the regression tests and performance comparison as gates. Agree customer-relevant dataset sizes,
type distribution, concurrency, transaction durations, read/write mix and budgets for throughput, tail
latency, allocation/GC and abort rate. Then rerun longer paired trials on a controlled machine followed
by representative application/EAI load, long-transaction and soak tests. Include realistic payloads,
hot roots, external effects and safe retry policies. No production acceptance claim is made here.

## Evidence and reproduction

The [benchmark guide](../strolch-jmh-benchmark/TRANSIENT-ISOLATION.md) documents setup and exact commands.
[Raw evidence](../strolch-jmh-benchmark/results/transient-2026-10-23) includes losslessly gzip-compressed paired JMH JSON/logs,
[environment and commands](../strolch-jmh-benchmark/results/transient-2026-10-23/environment.json),
and [regenerable summary CSV](../strolch-jmh-benchmark/results/transient-2026-10-23/summary.csv).
The customer-shaped evidence is in [its separate result directory](../strolch-jmh-benchmark/results/transient-customer-2026-10-23).
An [initial benchmark cleanup-order failure](../strolch-jmh-benchmark/results/harness-validation/README.md)
was fixed before rerunning the customer campaign; failed campaign scores are excluded.
JMH logs identify the actual loaded production class directory. The environment record includes the
compiled harness SHA-256 and the working-tree state for both sides.
