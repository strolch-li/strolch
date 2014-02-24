package li.strolch.persistence.inmemory;

import li.strolch.agent.api.StrolchRealm;
import li.strolch.persistence.api.AbstractTransaction;
import li.strolch.persistence.api.PersistenceHandler;
import li.strolch.persistence.api.TransactionResult;
import li.strolch.persistence.api.TransactionState;

public class InMemoryTransaction extends AbstractTransaction {

	private InMemoryPersistence persistenceHandler;

	public InMemoryTransaction(StrolchRealm realm, InMemoryPersistence persistenceHandler) {
		super(realm);
		this.persistenceHandler = persistenceHandler;
	}

	@Override
	protected void commit(TransactionResult txResult) throws Exception {
		txResult.setState(TransactionState.COMMITTED);
	}

	@Override
	protected void rollback(TransactionResult txResult) throws Exception {
		txResult.setState(TransactionState.ROLLED_BACK);
	}

	@Override
	public PersistenceHandler getPersistenceHandler() {
		return this.persistenceHandler;
	}
}
