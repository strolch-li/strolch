package li.strolch.runtime.test;

import li.strolch.model.Resource;
import li.strolch.runtime.component.StrolchComponent;

public class PersistenceHandlerTestImpl extends StrolchComponent implements PersistenceHandlerTest {

	public PersistenceHandlerTestImpl(String componentName) {
		super(componentName);
	}

	@Override
	public Resource getTestResource(String id, String name, String type) {

		Resource resource = new Resource(id, name, type);

		return resource;
	}
}
