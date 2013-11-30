package li.strolch.runtime.test.component;

import li.strolch.model.Resource;
import li.strolch.runtime.component.ComponentContainer;
import li.strolch.runtime.component.StrolchComponent;

public class PersistenceHandlerTestImpl extends StrolchComponent implements PersistenceHandlerTest {

	public PersistenceHandlerTestImpl(ComponentContainer container, String componentName) {
		super(container, componentName);
	}

	@Override
	public Resource getTestResource(String id, String name, String type) {

		Resource resource = new Resource(id, name, type);

		return resource;
	}
}
