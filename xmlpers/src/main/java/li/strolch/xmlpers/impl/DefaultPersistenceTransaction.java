/*
 * Copyright 2013 Robert von Burg <eitch@eitchnet.ch>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package li.strolch.xmlpers.impl;

import static li.strolch.utils.helper.StringHelper.formatNanoDuration;

import java.text.MessageFormat;
import java.util.*;

import li.strolch.utils.objectfilter.ObjectFilter;
import li.strolch.xmlpers.api.*;
import li.strolch.xmlpers.objref.LockableObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class DefaultPersistenceTransaction implements PersistenceTransaction {

	private static final Logger logger = LoggerFactory.getLogger(DefaultPersistenceTransaction.class);

	private final PersistenceManager manager;
	private final boolean verbose;

	private final ObjectFilter objectFilter;
	private final ObjectDao objectDao;
	private final MetadataDao metadataDao;

	private FileDao fileDao;

	private TransactionCloseStrategy closeStrategy;

	private TransactionState state;
	private long startTime;
	private Date startTimeDate;
	private TransactionResult txResult;

	private Set<LockableObject> lockedObjects;

	public DefaultPersistenceTransaction(PersistenceManager manager, IoMode ioMode, boolean verbose,
			boolean allowOverwriteOnCreate) {
		this.startTime = System.nanoTime();
		this.startTimeDate = new Date();
		this.manager = manager;
		this.verbose = verbose;
		this.objectFilter = new ObjectFilter();
		this.fileDao = new FileDao(this, manager.getPathBuilder(), verbose, allowOverwriteOnCreate);
		this.objectDao = new ObjectDao(this, this.fileDao, this.objectFilter);
		this.metadataDao = new MetadataDao(manager.getPathBuilder(), this, verbose);

		this.closeStrategy = TransactionCloseStrategy.COMMIT;
		this.state = TransactionState.OPEN;
		this.lockedObjects = new HashSet<>();
	}

	@Override
	public PersistenceManager getManager() {
		return this.manager;
	}

	@Override
	public void setTransactionResult(TransactionResult txResult) throws IllegalStateException {
		if (this.txResult != null) {
			String msg = "The transaction already has a result set!"; //$NON-NLS-1$
			throw new IllegalStateException(msg);
		}
		this.txResult = txResult;
	}

	@Override
	public TransactionResult getTransactionResult() throws IllegalStateException {
		return this.txResult;
	}

	@Override
	public boolean hasTransactionResult() {
		return this.txResult != null;
	}

	@Override
	public ObjectDao getObjectDao() {
		return this.objectDao;
	}

	@Override
	public MetadataDao getMetadataDao() {
		return this.metadataDao;
	}

	@Override
	public FileDao getFileDao() {
		return this.fileDao;
	}

	@Override
	public void setCloseStrategy(TransactionCloseStrategy closeStrategy) {
		this.closeStrategy = closeStrategy;
	}

	@Override
	public void close() throws XmlPersistenceException {
		this.closeStrategy.close(this);
	}

	@Override
	public void autoCloseableRollback() {
		try {
			long start = System.nanoTime();
			if (this.state == TransactionState.COMMITTED)
				throw new IllegalStateException("Transaction has already been committed!"); //$NON-NLS-1$

			if (this.state != TransactionState.ROLLED_BACK) {
				this.state = TransactionState.ROLLED_BACK;

				long end = System.nanoTime();
				long txDuration = end - this.startTime;
				long closeDuration = end - start;

				if (this.txResult != null) {
					this.txResult.clear();
					this.txResult.setState(this.state);
					this.txResult.setStartTime(this.startTimeDate);
					this.txResult.setTxDuration(txDuration);
					this.txResult.setCloseDuration(closeDuration);
					this.txResult.setModificationByKey(Collections.emptyMap());
				}
			}
		} finally {
			// clean up
			this.objectFilter.clearCache();
			releaseAllLocks();
		}
	}

	private void internalCommit() {

		Set<String> keySet = this.objectFilter.keySet();
		Map<String, ModificationResult> modifications;
		if (this.txResult == null)
			modifications = null;
		else
			modifications = new HashMap<>(keySet.size());

		for (String key : keySet) {

			List<Object> removed = this.objectFilter.getRemoved(key);
			if (removed.isEmpty()) {
				if (this.verbose)
					logger.info("No objects removed in this tx."); //$NON-NLS-1$
			} else {
				if (this.verbose)
					logger.info(removed.size() + " objects removed in this tx."); //$NON-NLS-1$

				for (Object object : removed) {
					@SuppressWarnings("unchecked")
					PersistenceContext<Object> ctx = (PersistenceContext<Object>) object;
					this.fileDao.performDelete(ctx);
				}
			}

			List<Object> updated = this.objectFilter.getUpdated(key);
			if (updated.isEmpty()) {
				if (this.verbose)
					logger.info("No objects updated in this tx."); //$NON-NLS-1$
			} else {
				if (this.verbose)
					logger.info(updated.size() + " objects updated in this tx."); //$NON-NLS-1$

				for (Object object : updated) {
					@SuppressWarnings("unchecked")
					PersistenceContext<Object> ctx = (PersistenceContext<Object>) object;
					this.fileDao.performUpdate(ctx);
				}
			}

			List<Object> added = this.objectFilter.getAdded(key);
			if (added.isEmpty()) {
				if (this.verbose)
					logger.info("No objects added in this tx."); //$NON-NLS-1$
			} else {
				if (this.verbose)
					logger.info(added.size() + " objects added in this tx."); //$NON-NLS-1$

				for (Object object : added) {
					@SuppressWarnings("unchecked")
					PersistenceContext<Object> ctx = (PersistenceContext<Object>) object;
					this.fileDao.performCreate(ctx);
				}
			}

			if (modifications != null) {
				ModificationResult result = new ModificationResult(key, added, updated, removed);
				modifications.put(key, result);
			}
		}

		if (this.txResult != null) {
			this.txResult.clear();
			this.txResult.setState(TransactionState.COMMITTED);
			this.txResult.setModificationByKey(modifications);
		}
	}

	@Override
	public void autoCloseableCommit() throws XmlPersistenceException {

		long start = System.nanoTime();

		try {

			if (this.verbose) {
				String msg = "Committing {0} operations in TX...";//$NON-NLS-1$
				logger.info(MessageFormat.format(msg, this.objectFilter.sizeCache()));
			}

			internalCommit();

		} catch (Exception e) {
			logger.error("Failed to commit!", e);

			if (this.txResult == null) {

				long end = System.nanoTime();
				long txDuration = end - this.startTime;
				long closeDuration = end - start;

				String sb = "TX has failed after " + formatNanoDuration(txDuration) + " with close operation taking "
						+ formatNanoDuration(closeDuration);
				logger.error(sb);

				throw e;
			}

			this.txResult.clear();
			this.txResult.setState(TransactionState.FAILED);
			this.txResult.setModificationByKey(Collections.emptyMap());
			this.txResult.setFailCause(e);

		} finally {
			// clean up
			this.objectFilter.clearCache();
			releaseAllLocks();
		}

		long end = System.nanoTime();
		long txDuration = end - this.startTime;
		long closeDuration = end - start;

		if (this.txResult == null) {

			String sb = "TX was completed after " + formatNanoDuration(txDuration) + " with close operation taking "
					+ formatNanoDuration(closeDuration);
			logger.info(sb);

		} else {

			this.txResult.setStartTime(this.startTimeDate);
			this.txResult.setTxDuration(txDuration);
			this.txResult.setCloseDuration(closeDuration);

			if (this.txResult.getState() == TransactionState.FAILED) {
				String msg = "Failed to commit TX due to underlying exception: {0}";
				String causeMsg =
						this.txResult.getFailCause() == null ? null : this.txResult.getFailCause().getMessage();
				msg = MessageFormat.format(msg, causeMsg);
				throw new XmlPersistenceException(msg, this.txResult.getFailCause());
			}
		}
	}

	private void releaseAllLocks() {
		for (LockableObject lockedObject : this.lockedObjects) {
			lockedObject.releaseLock();
		}
		this.lockedObjects.clear();
	}

	@Override
	public boolean isOpen() {
		return this.state == TransactionState.OPEN;
	}

	@Override
	public void lock(LockableObject lockableObject) {
		lockableObject.lock();
		this.lockedObjects.add(lockableObject);
	}
}
