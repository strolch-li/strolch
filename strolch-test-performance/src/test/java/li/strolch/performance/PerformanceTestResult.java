package li.strolch.performance;

import li.strolch.service.api.ServiceResult;
import li.strolch.service.api.ServiceResultState;

public class PerformanceTestResult extends ServiceResult {
	private long nrOfTxs;

	public PerformanceTestResult(ServiceResultState state) {
		super(state);
	}

	public PerformanceTestResult(long nrOfTxs) {
		super(ServiceResultState.SUCCESS);
		this.nrOfTxs = nrOfTxs;
	}

	public long getNrOfTxs() {
		return this.nrOfTxs;
	}
}
