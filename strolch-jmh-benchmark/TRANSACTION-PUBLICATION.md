# Transaction publication benchmarks

Compare the replacement transaction implementation against unchanged develop `39d4416f0`. This harness measures transient in-memory transaction overhead; it does not measure PostgreSQL commit latency or production services.

The customer fixture contains the exact supplied 143,211 Resources and 146,982 Orders by type, plus ten synthetic Activities (290,203 total). The doubled fixture doubles every type count (580,406). Root contents and IDs are synthetic. Eight workers, disjoint updated roots, small parameter bags, no real service work and the 9:1 read/write mix are benchmark assumptions, not customer traffic measurements.

Every completed trial checks final stored revisions against acknowledged successful writes. Write comparisons use the `commits` counter, rather than attempted operations. Conflicts and lock timeouts are separate counters. The latency benchmark includes retries for the superseded implementation's typed conflicts; this implementation does not introduce such conflicts. Setup and fixture loading are outside the measured interval. Fixture loading explicitly locks every inserted root in both engines, so both retain comparable lock metadata. Without this step, baseline direct-map loading bypasses locks while the replacement correctly acquires them, which biases the live heap state. Earlier exploratory trials with unequal retained lock records are excluded. Each variant uses the same compiled JMH harness and external dependencies, with that variant's own compiled production classes.

See [measured results and validation](../docs/TransactionPublicationPerformance.md), including observed regressions, latency and the longer confirmation.

## Reproduction

Use JDK 25. Compile both checkouts, including the benchmark module:

```sh
mvn -pl strolch-jmh-benchmark -am test -DskipTests
mvn -pl strolch-jmh-benchmark dependency:build-classpath \
  -DexcludeGroupIds=li.strolch -Dmdep.outputFile=/tmp/strolch-external-cp.txt
python3 strolch-jmh-benchmark/scripts/compare-transient.py \
  --baseline /path/to/unchanged-develop --candidate "$PWD" \
  --external-classpath /tmp/strolch-external-cp.txt \
  --output strolch-jmh-benchmark/results/transaction-publication \
  --java /path/to/jdk25/bin/java --quick --cases \
  read-large write-million mixed-concurrent search-large \
  customer-product-single customer-product-eight customer-person-eight \
  customer-picking-eight customer-fromstock-single customer-fromstock-eight \
  customer-mixed-fromstock customer-read-fromstock customer-double-fromstock \
  customer-latency-fromstock
python3 strolch-jmh-benchmark/scripts/summarize-transient.py \
  strolch-jmh-benchmark/results/transaction-publication --compress
```

`--quick` uses two independent forks per variant, two one-second warmups and three one-second measurements. Omitting it uses three forks, five two-second warmups and five two-second measurements. `--cases` selects any named uniform/customer cases from the driver. `--smoke` only checks the harness and is not performance evidence.

Forks run sequentially, alternating variant order by case, with G1, a fixed 2 GiB heap, and eight active processors. Run on an otherwise idle machine. Raw JMH JSON, fork logs, environment metadata, exact commands, source revision, and a regenerated CSV are generated locally under the ignored `results/` directory; these outputs are not committed. Synthetic transient results cannot establish a production acceptance budget; actual contention, parameter bag sizes, customer services, EAI work, PostgreSQL latency and traffic mix need representative deployment measurements.

The longer confirmation uses the same command with `--cases customer-person-eight`, a separate output directory, and no `--quick` flag.
