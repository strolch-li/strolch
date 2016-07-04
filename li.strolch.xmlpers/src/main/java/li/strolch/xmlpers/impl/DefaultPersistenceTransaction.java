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

import java.text.MessageFormat;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import li.strolch.utils.helper.StringHelper;
import li.strolch.utils.objectfilter.ObjectFilter;
import li.strolch.xmlpers.api.FileDao;
import li.strolch.xmlpers.api.IoMode;
import li.strolch.xmlpers.api.MetadataDao;
import li.strolch.xmlpers.api.ModificationResult;
import li.strolch.xmlpers.api.ObjectDao;
import li.strolch.xmlpers.api.PersistenceContext;
import li.strolch.xmlpers.api.PersistenceRealm;
import li.strolch.xmlpers.api.PersistenceTransaction;
import li.strolch.xmlpers.api.TransactionCloseStrategy;
import li.strolch.xmlpers.api.TransactionResult;
import li.strolch.xmlpers.api.TransactionState;
import li.strolch.xmlpers.api.XmlPersistenceException;
import li.strolch.xmlpers.objref.ObjectReferenceCache;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public class DefaultPersistenceTransaction implements PersistenceTransaction {

	private static final Logger logger = LoggerFactory.getLogger(DefaultPersistenceTransaction.class);

	private final DefaultPersistenceRealm realm;
	private final boolean verbose;

	private final ObjectFilter objectFilter;
	private final ObjectDao objectDao;
	private final MetadataDao metadataDao;

	private FileDao fileDao;
	private IoMode ioMode;

	private TransactionCloseStrategy closeStrategy;

	private TransactionState state;
	private long startTime;
	private Date startTimeDate;
	private TransactionResult txResult;

	public DefaultPersistenceTransaction(DefaultPersistenceRealm realm, boolean verbose) {
		this.startTime = System.nanoTime();
		this.startTimeDate = new Date();
		this.realm = realm;
		this.verbose = verbose;
		this.objectFilter = new ObjectFilter();
		this.fileDao = new FileDao(this, realm.getPathBuilder(), verbose);
		this.objectDao = new ObjectDao(this, this.fileDao, this.objectFilter);
		this.metadataDao = new MetadataDao(realm.getPathBuilder(), this, verbose);

		this.closeStrategy = TransactionCloseStrategy.COMMIT;
		this.state = TransactionState.OPEN;
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
		if (isOpen()) {
			String msg = "The transaction is still open thus has no result yet! Either commit or rollback before calling this method"; //$NON-NLS-1$
			throw new IllegalStateException(msg);
		}
		return this.txResult;
	}

	@Override
	public PersistenceRealm getRealm() {
		return this.realm;
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
	public ObjectReferenceCache getObjectRefCache() {
		return this.realm.getObjectRefCache();
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
		long start = System.nanoTime();
		if (this.state == TransactionState.COMMITTED)
			throw new IllegalStateException("Transaction has already been committed!"); //$NON-NLS-1$

		if (this.state != TransactionState.ROLLED_BACK) {
			unlockObjectRefs();
			this.state = TransactionState.ROLLED_BACK;
			this.objectFilter.clearCache();

			long end = System.nanoTime();
			long txDuration = end - this.startTime;
			long closeDuration = end - start;

			if (this.txResult != null) {
				this.txResult.clear();
				this.txResult.setState(this.state);
				this.txResult.setStartTime(this.startTimeDate);
				this.txResult.setTxDuration(txDuration);
				this.txResult.setCloseDuration(closeDuration);
				this.txResult.setRealm(this.realm.getRealmName());
				this.txResult.setModificationByKey(Collections.<String, ModificationResult> emptyMap());
			}
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

		} catch (Exception e) {

			if (this.txResult == null) {

				long end = System.nanoTime();
				long txDuration = end - this.startTime;
				long closeDuration = end - start;

				StringBuilder sb = new StringBuilder();
				sb.append("TX has failed after "); //$NON-NLS-1$
				sb.append(StringHelper.formatNanoDuration(txDuration));
				sb.append(" with close operation taking "); //$NON-NLS-1$
				sb.append(StringHelper.formatNanoDuration(closeDuration));
				logger.info(sb.toString());

				throw e;
			}

			this.txResult.clear();
			this.txResult.setState(TransactionState.FAILED);
			this.txResult.setModificationByKey(Collections.<String, ModificationResult> emptyMap());
			this.txResult.setFailCause(e);

		} finally {

			// clean up
			unlockObjectRefs();
			this.objectFilter.clearCache();
		}

		long end = System.nanoTime();
		long txDuration = end - this.startTime;
		long closeDuration = end - start;

		if (this.txResult == null) {

			StringBuilder sb = new StringBuilder();
			sb.append("TX was completed after "); //$NON-NLS-1$
			sb.append(StringHelper.formatNanoDuration(txDuration));
			sb.append(" with close operation taking "); //$NON-NLS-1$
			sb.append(StringHelper.formatNanoDuration(closeDuration));
			logger.info(sb.toString());

		} else {

			this.txResult.setStartTime(this.startTimeDate);
			this.txResult.setTxDuration(txDuration);
			this.txResult.setCloseDuration(closeDuration);
			this.txResult.setRealm(this.realm.getRealmName());

			if (this.txResult.getState() == TransactionState.FAILED) {
				String msg = "Failed to commit TX due to underlying exception: {0}"; //$NON-NLS-1$
				String causeMsg = this.txResult.getFailCause() == null ? null : this.txResult.getFailCause()
						.getMessage();
				msg = MessageFormat.format(msg, causeMsg);
				throw new XmlPersistenceException(msg, this.txResult.getFailCause());
			}
		}
	}

	@SuppressWarnings("rawtypes")
	private void unlockObjectRefs() {
		List<PersistenceContext> lockedObjects = this.objectFilter.getAll(PersistenceContext.class);
		for (PersistenceContext lockedObject : lockedObjects) {
			lockedObject.getObjectRef().unlock();
		}
	}

	@Override
	public boolean isOpen() {
		return this.state == TransactionState.OPEN;
	}

	@Override
	public void setIoMode(IoMode ioMode) {
		this.ioMode = ioMode;
	}

	@Override
	public IoMode getIoMode() {
		return this.ioMode;
	}
}
