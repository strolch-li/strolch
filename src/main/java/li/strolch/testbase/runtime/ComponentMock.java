package li.strolch.testbase.runtime;

import li.strolch.runtime.component.StrolchComponent;
import li.strolch.runtime.configuration.StrolchConfiguration;

public interface ComponentMock {

	public StrolchComponent getComponent();
	
	public void mock(StrolchConfiguration configuration);
}
