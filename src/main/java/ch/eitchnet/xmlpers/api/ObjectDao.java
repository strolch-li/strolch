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
package ch.eitchnet.xmlpers.api;

import static ch.eitchnet.xmlpers.util.AssertionUtil.assertIsIdRef;
import static ch.eitchnet.xmlpers.util.AssertionUtil.assertIsNotIdRef;
import static ch.eitchnet.xmlpers.util.AssertionUtil.assertIsNotRootRef;
import static ch.eitchnet.xmlpers.util.AssertionUtil.assertNotNull;
import static ch.eitchnet.xmlpers.util.AssertionUtil.assertObjectRead;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import ch.eitchnet.utils.objectfilter.ObjectFilter;
import ch.eitchnet.xmlpers.objref.ObjectRef;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public class ObjectDao {

	private final ObjectFilter objectFilter;
	private final FileDao fileDao;
	private final PersistenceTransaction tx;

	public ObjectDao(PersistenceTransaction tx, FileDao fileDao, ObjectFilter objectFilter) {
		this.tx = tx;
		this.fileDao = fileDao;
		this.objectFilter = objectFilter;
	}

	public <T> void add(T object) {
		assertNotClosed();
		assertNotNull(object);
		this.objectFilter.add(object);
	}

	@SuppressWarnings("unchecked")
	public <T> void addAll(List<T> objects) {
		assertNotClosed();
		assertNotNull(objects);
		if (!objects.isEmpty())
			this.objectFilter.addAll((List<Object>) objects);
	}

	public <T> void update(T object) {
		assertNotClosed();
		assertNotNull(object);
		this.objectFilter.update(object);
	}

	@SuppressWarnings("unchecked")
	public <T> void updateAll(List<T> objects) {
		assertNotClosed();
		assertNotNull(objects);
		if (!objects.isEmpty())
			this.objectFilter.updateAll((List<Object>) objects);
	}

	public <T> void remove(T object) {
		assertNotClosed();
		assertNotNull(object);
		this.objectFilter.remove(object);
	}

	@SuppressWarnings("unchecked")
	public <T> void removeAll(List<T> objects) {
		assertNotClosed();
		assertNotNull(objects);
		if (!objects.isEmpty())
			this.objectFilter.removeAll((List<Object>) objects);
	}

	public <T> void removeById(ObjectRef objectRef) {
		assertNotClosed();
		assertIsIdRef(objectRef);
		this.objectFilter.remove(objectRef);
	}

	public <T> void removeAll(ObjectRef parentRef) {
		assertNotClosed();
		assertIsNotIdRef(parentRef);
		assertIsNotRootRef(parentRef);

		Set<String> keySet = queryKeySet(parentRef);
		for (String id : keySet) {

			ObjectRef childRef = parentRef.getChildIdRef(this.tx, id);
			PersistenceContext<T> childCtx = childRef.<T> createPersistenceContext(this.tx);

			this.objectFilter.remove(childCtx);
		}
	}

	public <T> T queryById(ObjectRef objectRef) {
		assertNotClosed();
		assertIsIdRef(objectRef);
		PersistenceContext<T> ctx = objectRef.<T> createPersistenceContext(this.tx);
		this.fileDao.performRead(ctx);
		return ctx.getObject();
	}

	public <T> List<T> queryAll(ObjectRef parentRef) {
		assertNotClosed();
		assertIsNotIdRef(parentRef);

		MetadataDao metadataDao = this.tx.getMetadataDao();
		Set<String> keySet = metadataDao.queryKeySet(parentRef);

		List<T> result = new ArrayList<>();
		for (String id : keySet) {

			ObjectRef childRef = parentRef.getChildIdRef(this.tx, id);
			PersistenceContext<T> childCtx = childRef.createPersistenceContext(this.tx);

			this.fileDao.performRead(childCtx);
			assertObjectRead(childCtx);
			result.add(childCtx.getObject());
		}

		return result;
	}

	public <T> Set<String> queryKeySet(ObjectRef parentRef) {
		assertNotClosed();
		assertIsNotIdRef(parentRef);

		MetadataDao metadataDao = this.tx.getMetadataDao();
		Set<String> keySet = metadataDao.queryKeySet(parentRef);
		return keySet;
	}

	public <T> long querySize(ObjectRef parentRef) {
		assertNotClosed();
		assertIsNotIdRef(parentRef);

		MetadataDao metadataDao = this.tx.getMetadataDao();
		long size = metadataDao.querySize(parentRef);
		return size;
	}

	private void assertNotClosed() {
		if (!this.tx.isOpen())
			throw new IllegalStateException("Transaction has been closed and thus no operation can be performed!"); //$NON-NLS-1$
	}
}
