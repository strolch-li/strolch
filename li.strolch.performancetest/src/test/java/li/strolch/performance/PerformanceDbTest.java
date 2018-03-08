package li.strolch.performance;

import li.strolch.privilege.model.Certificate;
import li.strolch.service.api.ServiceHandler;
import org.junit.Test;

public abstract class PerformanceDbTest extends PerformanceTest {

	@Test
	public void runPerformanceTestCached() {
		Certificate certificate = runtime().getPrivilegeHandler().authenticate("cached", "cached".toCharArray());
		ServiceHandler svcHandler = runtime().getServiceHandler();
		svcHandler.doService(certificate, new PerformanceTestService(), argInstance());
	}
}
