package li.strolch.agent.api;

import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Certificate;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public abstract class AbstractModelObserver {

	protected static final Logger logger = LoggerFactory.getLogger(AbstractModelObserver.class);

	private final ComponentContainer container;

	public AbstractModelObserver(ComponentContainer container) {
		this.container = container;
	}

	public ComponentContainer getContainer() {
		return this.container;
	}

	protected StrolchTransaction openTx(Certificate cert) {
		return this.container.getRealm(cert).openTx(cert, getClass(), false);
	}
}
