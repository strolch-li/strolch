# Transaction publication: performance and validation

The replacement removes the rejected design's whole-type copy and unrelated-writer revision rejection. It adds private model changes and publication after persistence commit. The measured costs below remain relevant to the merge decision: this report does not establish production performance acceptance or claim every workload becomes faster.

## Source and protocol

- Baseline production: unchanged develop `39d4416f0bb4f6b340f151aa9fc1e5f2d2e13bfe`.
- Replacement production: `81eb6470b`; final benchmark harness/fixture: `49d6f49f6`. The later SonarCloud cleanup extracts counting, persistence-preparation and failure-handling helpers, and updates benchmark exception classification and setup logging. These numbers remain measurements of the revisions named here; that cleanup is regression-tested but is not a new performance campaign.
- Same compiled JMH 1.37 harness and external dependencies for both engines; each uses its own compiled local production classes. Every completed trial verifies stored revisions against acknowledged successful writes.
- Exact supplied customer type counts: 143,211 Resources and 146,982 Orders, plus ten explicitly synthetic Activities: 290,203 total. Doubled fixture: 580,406 roots. Contents, IDs, worker count, disjoint updated roots, small parameter bags and 9:1 mix are synthetic assumptions.
- Fixture loading explicitly locks all roots for both engines. Idle lock records are retained for an hour; baseline direct-map loading otherwise bypasses them, producing unequal live heap state. Earlier exploratory trials with unequal fixture locks are excluded from this report.
- JDK 25.0.4+7, G1, fixed 2 GiB heap, eight active processors. Sequential variants, alternating order by case. No compilation or tests run alongside measurements. This remains a shared development laptop, not a controlled production host.
- Quick campaign: 14 paired scenarios, 28 result sets, 56 completed forks; two forks per variant, two one-second warmups and three one-second measurements. Short runs have broad confidence intervals and variable tails. Inspect `primaryError99_9` and per-fork raw data when reproducing the campaign; small differences are not proof of equivalence or improvement.

## Quick campaign

Rates are successful commits/s for write-only cases, successful operations/s (reads plus commits) for mixed cases, and reads or searches/s for read-only cases. Allocation is bytes per measured operation; no writes were rejected and no lock timeouts occurred.

| Workload | Develop useful/s | Replacement useful/s | Change | Develop B/op | Replacement B/op |
|---|---:|---:|---:|---:|---:|
| 100K roots, one reader | 892,176 | 970,482 | +8.8% | 1,688 | 1,880 |
| 1M roots, one independent writer | 179,263 | 156,327 | -12.8% | 10,849 | 11,062 |
| 100K roots, eight workers, 9:1 reads/writes | 877,002 | 803,523 | -8.4% | 2,586 | 2,665 |
| Search 100K roots, one reader | 131 | 164 | +25.2% | 803,256 | 803,733 |
| Customer Product, one writer | 170,114 | 175,914 | +3.4% | 10,881 | 11,385 |
| Customer Product, eight writers | 344,966 | 328,477 | -4.8% | 10,981 | 11,408 |
| Customer Person, eight writers | 402,382 | 320,065 | -20.5% | 11,074 | 11,538 |
| Customer PickingItem, eight writers | 307,660 | 261,533 | -15.0% | 11,016 | 11,213 |
| Customer FromStock, one writer | 142,726 | 143,066 | +0.2% | 10,945 | 11,414 |
| Customer FromStock, eight writers | 256,999 | 243,796 | -5.1% | 10,991 | 11,512 |
| Customer FromStock, eight workers, 9:1 reads/writes | 823,892 | 638,447 | -22.5% | 2,616 | 2,724 |
| Customer FromStock, eight readers | 1,065,443 | 1,105,074 | +3.7% | 1,704 | 1,704 |
| Doubled customer FromStock, eight writers | 267,786 | 249,871 | -6.7% | 11,195 | 11,496 |

Sampled successful FromStock write latency, eight independent writers (microseconds):

| Percentile | Develop | Replacement |
|---|---:|---:|
| p50 | 5.92 | 5.77 |
| p95 | 18.50 | 28.38 |
| p99 | 51.52 | 106.62 |

Latency includes retries for typed transaction conflicts; neither engine reported a conflict in these runs. The benchmark fails on arbitrary errors or unexpected timeouts instead of treating them as successful operations.

## Longer confirmation

The Person/eight-writer case received a longer confirmation because its quick-run baseline varied substantially. The confirmation uses three forks per variant, five two-second warmups and five two-second measurements. Both quick and confirmation results are retained.

| Customer Person, eight writers | Develop | Replacement |
|---|---:|---:|
| Successful commits/s | 278,425 | 241,321 |
| Allocated B/op | 11,060 | 11,442 |
| Primary throughput 99.9% error (ops/s) | 92,992 | 105,048 |

Successful-write throughput changed by -13.3% in the longer run. These are synthetic transient measurements, not PostgreSQL commit or customer-service timings.

## Interpretation and limits

Write allocation remains proportional to the changed roots, around 11 KB for a single-root update, including the million-root and doubled customer fixtures. It does not grow to megabytes with the number of roots in the type. Searches still allocate their existing result capture, proportional to result size.

Rates and latency vary by workload. The overhead of overlays and publication coordination is measurable; the table includes regressions and improvements. Do not replace a customer performance budget with a synthetic aggregate average. No production traffic mix, service durations, acceptable regression threshold, or PostgreSQL latency distribution was supplied. These tests cannot establish end-to-end cached/PostgreSQL throughput, multi-node behavior, or production acceptance. Representative application and database measurements are still needed for that decision.

The transaction contract intentionally uses explicit pessimistic locking for business dependencies. It does not automatically prevent write skew, phantoms or mixed-version reads across separate calls. Administrative bulk clears require quiesced traffic. Historical versions for recreated identifiers can be provisional until persistence preparation. See [the contract and recovery behavior](TRANSACTION-PUBLICATION.md).

## Correctness validation

Full service dependency reactor: **926 tests discovered, 898 executed, zero failures/errors, 28 skips**, including existing PostgreSQL XML/JSON/history and EclipseStore tests. The new transaction suite contains 66 parameterized cases (11 assumptions skipped for inapplicable modes), plus two forced-publication tests. It covers local flush visibility and rollback, pending insert/update/delete searches, independent writers, root kinds, reset, bulk clear, history undo/revert, no SQL during flush, SQL-before-memory ordering, write failure, lost commit response, acknowledged-commit cleanup failure, and post-commit publication failure. History updates reuse locked versions without another latest-version SQL lookup.

```sh
mvn -pl strolch-service -am test \
  -Ddb.hostOverride=localhost:55432 -Dtransaction.postgres=true
mvn -pl strolch-jmh-benchmark -am test -DskipTests
```

All PostgreSQL integration databases were disposable databases on local port 55432; the existing server on port 5432 was not used. This is not crash-durability or universal ACID certification.

## Reproduction

See the [reproduction guide](../strolch-jmh-benchmark/TRANSACTION-PUBLICATION.md) for the benchmark workloads, commands, and summary generation. The measured summaries and methodology are retained in this document. Raw JMH JSON, logs, environment metadata, and CSV files are generated locally under the ignored `strolch-jmh-benchmark/results/` directory and are not version controlled.
