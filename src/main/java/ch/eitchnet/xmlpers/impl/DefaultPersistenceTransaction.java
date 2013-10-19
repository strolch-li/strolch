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
import java.util.List;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ch.eitchnet.utils.helper.StringHelper;
import ch.eitchnet.utils.objectfilter.ObjectFilter;
import ch.eitchnet.xmlpers.api.FileDao;
import ch.eitchnet.xmlpers.api.IoMode;
import ch.eitchnet.xmlpers.api.MetadataDao;
import ch.eitchnet.xmlpers.api.ObjectDao;
import ch.eitchnet.xmlpers.api.PersistenceContext;
import ch.eitchnet.xmlpers.api.PersistenceRealm;
import ch.eitchnet.xmlpers.api.PersistenceTransaction;
import ch.eitchnet.xmlpers.api.TransactionCloseStrategy;
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

	private boolean committed;
	private boolean closed;

	private IoMode ioMode;

	private TransactionCloseStrategy closeStrategy;

	public DefaultPersistenceTransaction(DefaultPersistenceRealm realm, boolean verbose) {
		this.realm = realm;
		this.verbose = verbose;
		this.objectFilter = new ObjectFilter();
		this.fileDao = new FileDao(this, realm.getPathBuilder(), verbose);
		this.objectDao = new ObjectDao(this, this.fileDao, this.objectFilter);
		this.metadataDao = new MetadataDao(realm.getPathBuilder(), this, verbose);

		this.closeStrategy = TransactionCloseStrategy.COMMIT;
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
		if (this.committed)
			throw new IllegalStateException("Transaction has already been committed!"); //$NON-NLS-1$

		if (!this.closed) {
			this.closed = true;
			this.objectFilter.clearCache();
		}
	}

	@Override
	public void autoCloseableCommit() throws XmlPersistenceException {

		long start = System.nanoTime();

		try {

			Set<String> keySet = this.objectFilter.keySet();

			if (this.verbose) {
				String msg = "Committing {0} operations in TX...";//$NON-NLS-1$
				logger.info(MessageFormat.format(msg, keySet.size()));
			}

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
			}

			long end = System.nanoTime();
			logger.info("TX completed in " + StringHelper.formatNanoDuration(end - start)); //$NON-NLS-1$

		} catch (Exception e) {

			long end = System.nanoTime();
			logger.info("TX failed after " + StringHelper.formatNanoDuration(end - start)); //$NON-NLS-1$

			throw e;

		} finally {
			// clean up
			this.objectFilter.clearCache();
			this.committed = true;
		}
	}

	@Override
	public boolean isOpen() {
		return !this.closed && !this.committed;
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
