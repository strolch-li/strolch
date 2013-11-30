package li.strolch.runtime.test.component;

import li.strolch.runtime.component.ComponentContainer;
import li.strolch.runtime.component.StrolchComponent;

public class PostInitializerTestImpl extends StrolchComponent implements PostInitializerTest {

	public PostInitializerTestImpl(ComponentContainer container, String componentName) {
		super(container, componentName);
	}
}
