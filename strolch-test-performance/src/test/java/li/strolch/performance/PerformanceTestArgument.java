package li.strolch.performance;

import java.util.concurrent.TimeUnit;

import li.strolch.service.api.ServiceArgument;

public class PerformanceTestArgument extends ServiceArgument {

	public final long duration = 15;
	public final TimeUnit unit = TimeUnit.SECONDS;
	public int nrOfElements = 1;
}
