package li.strolch.persistence.impl;

import li.strolch.persistence.api.StrolchPersistenceException;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.persistence.api.TransactionCloseStrategy;
import ch.eitchnet.xmlpers.api.PersistenceTransaction;

public class XmlStrolchTransaction implements StrolchTransaction {

	private PersistenceTransaction tx;
	private TransactionCloseStrategy closeStrategy;

	public XmlStrolchTransaction(PersistenceTransaction tx) {
		this.tx = tx;
		this.closeStrategy = TransactionCloseStrategy.COMMIT;
	}

	PersistenceTransaction getTx() {
		return this.tx;
	}

	@Override
	public void setCloseStrategy(TransactionCloseStrategy closeStrategy) {
		this.closeStrategy = closeStrategy;
	}

	@Override
	public void autoCloseableCommit() {
		this.tx.autoCloseableCommit();
	}

	@Override
	public void autoCloseableRollback() {
		this.tx.autoCloseableRollback();
	}

	@Override
	public void close() throws StrolchPersistenceException {
		this.closeStrategy.close(this);
	}

	@Override
	public boolean isOpen() {
		return this.tx.isOpen();
	}
}
