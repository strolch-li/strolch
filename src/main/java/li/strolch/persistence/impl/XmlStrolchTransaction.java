package li.strolch.persistence.impl;

import java.util.Set;

import li.strolch.model.StrolchElement;
import li.strolch.persistence.api.StrolchPersistenceException;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.persistence.api.TransactionCloseStrategy;
import li.strolch.runtime.observer.ObserverHandler;
import ch.eitchnet.xmlpers.api.ModificationResult;
import ch.eitchnet.xmlpers.api.PersistenceTransaction;
import ch.eitchnet.xmlpers.api.TransactionResult;

public class XmlStrolchTransaction implements StrolchTransaction {

	private ObserverHandler observerHandler;
	private boolean suppressUpdates;
	private PersistenceTransaction tx;
	private TransactionCloseStrategy closeStrategy;
	private TransactionResult txResult;

	public XmlStrolchTransaction(PersistenceTransaction tx) {
		this.suppressUpdates = false;
		this.tx = tx;
		this.closeStrategy = TransactionCloseStrategy.COMMIT;
	}

	/**
	 * @param observerHandler
	 *            the observerHandler to set
	 */
	public void setObserverHandler(ObserverHandler observerHandler) {
		this.observerHandler = observerHandler;
	}

	/**
	 * @param suppressUpdates
	 *            the suppressUpdates to set
	 */
	public void setSuppressUpdates(boolean suppressUpdates) {
		this.suppressUpdates = suppressUpdates;
	}

	/**
	 * @return the suppressUpdates
	 */
	public boolean isSuppressUpdates() {
		return this.suppressUpdates;
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

		if (!this.suppressUpdates && this.observerHandler != null) {
			this.txResult = new TransactionResult();
			this.tx.setTransactionResult(this.txResult);
		}

		this.tx.autoCloseableCommit();

		if (!this.suppressUpdates && this.observerHandler != null) {

			Set<String> keys = this.txResult.getKeys();
			for (String key : keys) {
				ModificationResult modificationResult = this.txResult.getModificationResult(key);

				this.observerHandler.add(key, modificationResult.<StrolchElement> getCreated());
				this.observerHandler.update(key, modificationResult.<StrolchElement> getUpdated());
				this.observerHandler.remove(key, modificationResult.<StrolchElement> getDeleted());
			}
		}
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
