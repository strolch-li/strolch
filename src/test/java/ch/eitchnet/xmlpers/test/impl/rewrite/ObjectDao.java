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

import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import ch.eitchnet.utils.helper.StringHelper;
import ch.eitchnet.utils.objectfilter.ObjectFilter;
import ch.eitchnet.xmlpers.api.PersistenceContext;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public class ObjectDao {

	private final PersistenceTransaction tx;
	private final ObjectFilter objectFilter;
	private final FileDao fileDao;
	private boolean closed;

	public ObjectDao(PersistenceTransaction tx, FileDao fileDao, ObjectFilter objectFilter) {
		this.tx = tx;
		this.fileDao = fileDao;
		this.objectFilter = objectFilter;
	}

	public void add(Object object) {
		assertNotClosed();
		assertNotNull(object);
		this.objectFilter.add(object);
	}

	public <T> void update(Object object) {
		assertNotClosed();
		assertNotNull(object);
		this.objectFilter.update(object);
	}

	public <T> void remove(Object object) {
		assertNotClosed();
		assertNotNull(object);
		this.objectFilter.remove(object);
	}

	public <T> void removeById(PersistenceContext<T> context) {
		assertNotClosed();
		assertHasType(context);
		assertHasId(context);
		this.objectFilter.remove(context.clone());
	}

	public <T> void removeAll(PersistenceContext<T> context) {
		assertNotClosed();
		assertHasType(context);

		Set<String> keySet = queryKeySet(context);
		for (String id : keySet) {
			PersistenceContext<T> clone = context.clone();
			clone.setId(id);
			this.objectFilter.remove(clone);
		}
	}

	public <T> T queryById(PersistenceContext<T> context) {
		assertNotClosed();
		assertHasType(context);
		assertHasId(context);
		this.fileDao.performRead(context);
		return context.getObject();
	}

	public <T> List<T> queryAll(PersistenceContext<T> context) {
		assertNotClosed();
		assertHasType(context);

		MetadataDao metadataDao = this.tx.getMetadataDao();
		Set<String> keySet;
		if (context.hasSubType())
			keySet = metadataDao.queryKeySet(context.getType(), context.getSubType());
		else
			keySet = metadataDao.queryKeySet(context.getType());

		List<T> result = new ArrayList<>();
		PersistenceContext<T> readContext = context.clone();
		for (String id : keySet) {
			readContext.setId(id);
			this.fileDao.performRead(readContext);
			assertObjectRead(readContext);
			result.add(readContext.getObject());
		}

		return result;
	}

	public <T> Set<String> queryKeySet(PersistenceContext<T> context) {
		assertNotClosed();
		assertHasType(context);

		assertNotClosed();
		assertHasType(context);

		MetadataDao metadataDao = this.tx.getMetadataDao();
		Set<String> keySet;
		if (context.hasSubType())
			keySet = metadataDao.queryKeySet(context.getType(), context.getSubType());
		else
			keySet = metadataDao.queryKeySet(context.getType());

		return keySet;
	}

	public <T> long querySize(PersistenceContext<T> context) {
		assertNotClosed();
		assertHasType(context);

		assertNotClosed();
		assertHasType(context);

		MetadataDao metadataDao = this.tx.getMetadataDao();
		long size;
		if (context.hasSubType())
			size = metadataDao.querySize(context.getType(), context.getSubType());
		else
			size = metadataDao.querySize(context.getType());

		return size;
	}

	public void rollback() {
		this.objectFilter.clearCache();
		this.closed = true;
	}

	private <T> void assertNotNull(Object object) {
		if (object == null)
			throw new RuntimeException("Object may not be null!"); //$NON-NLS-1$
	}

	private <T> void assertObjectRead(PersistenceContext<T> context) {
		if (context.getObject() == null) {
			String msg = "Failed to read object with for {0} / {1} / {2}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, context.getType(), context.getSubType(), context.getId());
			throw new RuntimeException(msg);
		}
	}

	private <T> void assertHasType(PersistenceContext<T> context) {
		if (StringHelper.isEmpty(context.getType()))
			throw new RuntimeException("Type is always needed on a persistence context!"); //$NON-NLS-1$
	}

	private <T> void assertHasId(PersistenceContext<T> context) {
		if (StringHelper.isEmpty(context.getId()))
			throw new RuntimeException("Id is not set on persistence context!"); //$NON-NLS-1$
	}

	private void assertNotClosed() {
		if (this.closed || !this.tx.isOpen())
			throw new IllegalStateException("Transaction has been closed and thus no operation can be performed!"); //$NON-NLS-1$
	}
}
