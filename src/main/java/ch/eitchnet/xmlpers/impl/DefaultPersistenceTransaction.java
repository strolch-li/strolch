/*
 * Copyright (c) 2012, Robert von Burg
 *
 * All rights reserved.
 *
 * This file is part of the XXX.
 *
 *  XXX is free software: you can redistribute 
 *  it and/or modify it under the terms of the GNU General Public License as 
 *  published by the Free Software Foundation, either version 3 of the License, 
 *  or (at your option) any later version.
 *
 *  XXX is distributed in the hope that it will 
 *  be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with XXX.  If not, see 
 *  <http://www.gnu.org/licenses/>.
 */
package ch.eitchnet.xmlpers.impl;

import java.text.MessageFormat;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ch.eitchnet.utils.helper.StringHelper;
import ch.eitchnet.utils.objectfilter.ObjectFilter;
import ch.eitchnet.xmlpers.api.FileDao;
import ch.eitchnet.xmlpers.api.IoMode;
import ch.eitchnet.xmlpers.api.MetadataDao;
import ch.eitchnet.xmlpers.api.ModificationResult;
import ch.eitchnet.xmlpers.api.ObjectDao;
import ch.eitchnet.xmlpers.api.PersistenceContext;
import ch.eitchnet.xmlpers.api.PersistenceRealm;
import ch.eitchnet.xmlpers.api.PersistenceTransaction;
import ch.eitchnet.xmlpers.api.TransactionCloseStrategy;
import ch.eitchnet.xmlpers.api.TransactionResult;
import ch.eitchnet.xmlpers.api.TransactionState;
import ch.eitchnet.xmlpers.api.XmlPersistenceException;
import ch.eitchnet.xmlpers.objref.ObjectReferenceCache;

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

			this.txResult.clear();
			this.txResult.setState(this.state);
			this.txResult.setStartTime(this.startTimeDate);
			this.txResult.setTxDuration(txDuration);
			this.txResult.setCloseDuration(closeDuration);
			this.txResult.setRealm(this.realm.getRealmName());
			this.txResult.setModificationByKey(Collections.<String, ModificationResult> emptyMap());
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
				msg = MessageFormat.format(msg, this.txResult.getFailCause().getMessage());
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
