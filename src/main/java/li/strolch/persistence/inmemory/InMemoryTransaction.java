package li.strolch.persistence.inmemory;

import java.util.Date;
import java.util.Set;

import li.strolch.agent.impl.StrolchRealm;
import li.strolch.model.StrolchElement;
import li.strolch.persistence.api.AbstractTransaction;
import li.strolch.persistence.api.ModificationResult;
import li.strolch.persistence.api.PersistenceHandler;
import li.strolch.persistence.api.StrolchPersistenceException;
import li.strolch.persistence.api.TransactionCloseStrategy;
import li.strolch.persistence.api.TransactionResult;
import li.strolch.persistence.api.TransactionState;
import li.strolch.runtime.observer.ObserverHandler;
import ch.eitchnet.utils.helper.StringHelper;

public class InMemoryTransaction extends AbstractTransaction {

	private InMemoryPersistence persistenceHandler;

	private TransactionCloseStrategy closeStrategy;
	private ObserverHandler observerHandler;
	private boolean suppressUpdates;

	private long startTime;
	private Date startTimeDate;
	private TransactionResult txResult;
	private boolean open;

	public InMemoryTransaction(StrolchRealm realm, InMemoryPersistence persistenceHandler) {
		super(realm);
		this.persistenceHandler = persistenceHandler;
		this.startTime = System.nanoTime();
		this.startTimeDate = new Date();
		this.suppressUpdates = false;
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

	@Override
	public void setCloseStrategy(TransactionCloseStrategy closeStrategy) {
		this.closeStrategy = closeStrategy;
	}

	@Override
	public void autoCloseableCommit() {

		if (logger.isDebugEnabled()) {
			logger.info("Committing TX..."); //$NON-NLS-1$
		}

		long start = System.nanoTime();
		this.txResult = new TransactionResult();

		try {

			this.txResult.setState(TransactionState.COMMITTED);

		} catch (Exception e) {

			this.txResult.setState(TransactionState.FAILED);
			long end = System.nanoTime();
			long txDuration = end - this.startTime;
			long closeDuration = end - start;
			StringBuilder sb = new StringBuilder();
			sb.append("TX has failed after "); //$NON-NLS-1$
			sb.append(StringHelper.formatNanoDuration(txDuration));
			sb.append(" with close operation taking "); //$NON-NLS-1$
			sb.append(StringHelper.formatNanoDuration(closeDuration));
			logger.info(sb.toString());

			throw new StrolchPersistenceException("Strolch Transaction failed due to " + e.getMessage(), e); //$NON-NLS-1$

		}

		long end = System.nanoTime();
		long txDuration = end - this.startTime;
		long closeDuration = end - start;

		this.txResult.setStartTime(this.startTimeDate);
		this.txResult.setTxDuration(txDuration);
		this.txResult.setCloseDuration(closeDuration);
		this.txResult.setRealm(getRealm().getRealm());

		StringBuilder sb = new StringBuilder();
		sb.append("TX was completed after "); //$NON-NLS-1$
		sb.append(StringHelper.formatNanoDuration(txDuration));
		sb.append(" with close operation taking "); //$NON-NLS-1$
		sb.append(StringHelper.formatNanoDuration(closeDuration));
		logger.info(sb.toString());

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
		long start = System.nanoTime();

		long end = System.nanoTime();
		long txDuration = end - this.startTime;
		long closeDuration = end - start;

		this.txResult = new TransactionResult();
		this.txResult.setState(TransactionState.ROLLED_BACK);
		this.txResult.setStartTime(this.startTimeDate);
		this.txResult.setTxDuration(txDuration);
		this.txResult.setCloseDuration(closeDuration);
		this.txResult.setRealm(getRealm().getRealm());
	}

	@Override
	public void close() throws StrolchPersistenceException {
		this.closeStrategy.close(this);
	}

	@Override
	public boolean isOpen() {
		return this.open;
	}

	@Override
	public PersistenceHandler getPersistenceHandler() {
		return this.persistenceHandler;
	}
}
