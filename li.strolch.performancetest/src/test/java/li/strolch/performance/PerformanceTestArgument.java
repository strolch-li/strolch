package li.strolch.performance;

import java.util.concurrent.TimeUnit;

import li.strolch.service.api.ServiceArgument;

public class PerformanceTestArgument extends ServiceArgument {
	private static final long serialVersionUID = 1L;
	public long duration = 15;
	public TimeUnit unit = TimeUnit.SECONDS;
	public int nrOfElements = 1;
}
