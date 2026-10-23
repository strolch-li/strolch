strolch-jmh-benchmark
======================

Strolch project to run JMH benchmarks.

To run use the following command:

   mvn clean verify -PrunBenchmark


## Transient transaction isolation comparison

For the shared-realm baseline-versus-PR workloads, successful-commit counters, allocation profiling
and latency percentiles, see [TRANSIENT-ISOLATION.md](TRANSIENT-ISOLATION.md). The new harness asserts
TRANSIENT mode; the older `runtime_transient` fixture uses EMPTY mode.
