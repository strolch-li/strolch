package li.strolch.runtime.configuration.model;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.StrolchComponent;

public class PersistenceHandlerImpl extends StrolchComponent implements PersistenceHandler {

	public PersistenceHandlerImpl(ComponentContainer container, String componentName) {
		super(container, componentName);
	}
}
