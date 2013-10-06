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
package ch.eitchnet.xmlpers.test.impl.rewrite;

import java.util.List;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ch.eitchnet.utils.helper.StringHelper;
import ch.eitchnet.utils.objectfilter.ObjectFilter;
import ch.eitchnet.xmlpers.api.PersistenceContext;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public class DefaultPersistenceTransaction implements PersistenceTransaction {

	private static final Logger logger = LoggerFactory.getLogger(DefaultPersistenceTransaction.class);

	private final FileDao fileDao;
	private final ObjectDao objectDao;
	private final MetadataDao metadataDao;
	private final ObjectFilter objectFilter;
	private final boolean verbose;

	private boolean closed;

	public DefaultPersistenceTransaction(FileDao fileDao, boolean verbose) {
		this.fileDao = fileDao;
		this.verbose = verbose;
		this.objectFilter = new ObjectFilter();
		this.objectDao = new ObjectDao(this, this.fileDao, this.objectFilter);
		this.metadataDao = new MetadataDao(this, this.fileDao);
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
	public void rollback() {
		this.closed = true;
		this.objectDao.rollback();
		this.objectFilter.clearCache();
	}

	@Override
	public void commit(PersistenceContextFactory persistenceContextFactory) {

		try {

			long start = System.nanoTime();
			if (this.verbose)
				logger.info("Committing TX..."); //$NON-NLS-1$

			Set<String> keySet = this.objectFilter.keySet();
			if (keySet.isEmpty())
				return;
			for (String key : keySet) {

				List<Object> removed = this.objectFilter.getRemoved(key);
				if (removed.isEmpty()) {
					if (this.verbose)
						logger.info("No objects removed in this tx."); //$NON-NLS-1$
				} else {
					if (this.verbose)
						logger.info(removed.size() + " objects removed in this tx."); //$NON-NLS-1$

					for (Object object : removed) {
						PersistenceContext<Object> context = persistenceContextFactory.createPersistenceContext(object);
						this.fileDao.performDelete(context);
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

						PersistenceContext<Object> context = persistenceContextFactory.createPersistenceContext(object);
						this.fileDao.performUpdate(context);
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

						PersistenceContext<Object> context = persistenceContextFactory.createPersistenceContext(object);
						this.fileDao.performCreate(context);
					}
				}
			}

			long end = System.nanoTime();
			logger.info("Completed TX in " + StringHelper.formatNanoDuration(end - start)); //$NON-NLS-1$

		} finally {
			// clean up
			this.objectFilter.clearCache();
			this.closed = true;
		}
	}

	@Override
	public boolean isClosed() {
		return this.closed;
	}
}
