package li.strolch.performance;

import org.junit.Test;

import li.strolch.privilege.model.Certificate;
import li.strolch.service.api.ServiceHandler;
import li.strolch.testbase.runtime.RuntimeMock;

public abstract class PerformanceTest {

	protected abstract RuntimeMock runtime();

	private PerformanceTestArgument argInstance() {
		PerformanceTestArgument arg = new PerformanceTestArgument();
		return arg;
	}

	@Test
	public void runPerformanceTestCached() {

		Certificate certificate = runtime().getPrivilegeHandler().authenticate("cached", "cached".toCharArray());

		ServiceHandler svcHandler = runtime().getServiceHandler();
		svcHandler.doService(certificate, new PerformanceTestService(), argInstance());
	}
}
