package li.strolch.persistence.inmemory;

import li.strolch.agent.api.StrolchRealm;
import li.strolch.persistence.api.AbstractTransaction;
import li.strolch.persistence.api.PersistenceHandler;
import li.strolch.persistence.api.TransactionResult;
import li.strolch.persistence.api.TransactionState;
import ch.eitchnet.privilege.model.Certificate;

public class InMemoryTransaction extends AbstractTransaction {

	private InMemoryPersistence persistenceHandler;

	public InMemoryTransaction(StrolchRealm realm, Certificate certificate, String action,
			InMemoryPersistence persistenceHandler) {
		super(realm, certificate, action);
		this.persistenceHandler = persistenceHandler;
	}

	@Override
	protected void writeChanges(TransactionResult txResult) throws Exception {
		txResult.setState(TransactionState.COMMITTED);
	}

	@Override
	protected void rollback(TransactionResult txResult) throws Exception {
		txResult.setState(TransactionState.ROLLED_BACK);
	}

	@Override
	protected void commit() throws Exception {
		// no-op
	}

	@Override
	public PersistenceHandler getPersistenceHandler() {
		return this.persistenceHandler;
	}
}
