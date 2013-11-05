package li.strolch.runtime.test;

import li.strolch.runtime.component.ComponentContainer;
import li.strolch.runtime.component.StrolchComponent;

public class ServiceHandlerTestImpl extends StrolchComponent implements ServiceHandlerTest {

	public ServiceHandlerTestImpl(ComponentContainer container, String componentName) {
		super(container, componentName);
	}

	@Override
	public ServiceResultTest doService() {
		return new ServiceResultTest(1);
	}
}
