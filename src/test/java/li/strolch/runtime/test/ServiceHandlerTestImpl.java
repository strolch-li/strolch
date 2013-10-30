package li.strolch.runtime.test;

import li.strolch.runtime.component.StrolchComponent;

public class ServiceHandlerTestImpl extends StrolchComponent implements ServiceHandlerTest {

	public ServiceHandlerTestImpl(String componentName) {
		super(componentName);
	}

	@Override
	public ServiceResultTest doService() {
		return new ServiceResultTest(1);
	}
}
