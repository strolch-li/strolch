# Harness validation failure (excluded from performance results)

The initial customer campaign stopped during final-state verification: JMH can tear down a shared
benchmark state on one worker before another worker's trial teardown reads from that state. The retained
`initial-teardown-race.log` shows this cleanup-order error on the baseline implementation. It is not
classified as a production transaction failure, isolation failure or a performance result.

The harness was repaired by registering workers at setup and checking all stored revisions inside the
single shared benchmark teardown, after JMH's iteration barrier and before destroying the runtime.
There are no per-worker final-state callbacks remaining. The customer campaign is rerun from the first
case after this repair; none of the incomplete campaign's scores are included in the customer report.
The older uniform campaign completed all trials and checks without this error, with its exact harness
source retained alongside its evidence. Teardown checks are outside timed benchmark operations.
