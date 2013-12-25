package li.strolch.persistence.mock;

import li.strolch.persistence.api.PersistenceHandler;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.runtime.StrolchConstants;
import li.strolch.runtime.agent.ComponentContainerImpl;
import li.strolch.runtime.agent.StrolchComponent;

public class PersistenceHandlerMock extends StrolchComponent implements PersistenceHandler {

	public PersistenceHandlerMock(ComponentContainerImpl container, String componentName) {
		super(container, componentName);
	}

	@Override
	public StrolchTransaction openTx() {
		return openTx(StrolchConstants.DEFAULT_REALM);
	}

	@Override
	public StrolchTransaction openTx(String realm) {
		return new TransactionMock(realm);
	}
}
