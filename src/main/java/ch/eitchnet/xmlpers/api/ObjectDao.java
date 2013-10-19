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
	private PersistenceContextFactoryDelegator ctxFactoryDelegator;

	public ObjectDao(PersistenceTransaction tx, FileDao fileDao, ObjectFilter objectFilter) {
		this.tx = tx;
		this.fileDao = fileDao;
		this.objectFilter = objectFilter;
		this.ctxFactoryDelegator = this.tx.getRealm().getCtxFactoryDelegator();
	}

	public <T> void add(T object) {
		assertNotClosed();
		assertNotNull(object);
		PersistenceContext<T> ctx = createCtx(object);
		ctx.setObject(object);
		ctx.getObjectRef().lock();
		this.objectFilter.add(object.getClass().getName(), ctx);
	}

	public <T> void addAll(List<T> objects) {
		assertNotClosed();
		assertNotNull(objects);
		if (!objects.isEmpty()) {
			for (T object : objects) {
				PersistenceContext<T> ctx = createCtx(object);
				ctx.setObject(object);
				ctx.getObjectRef().lock();
				this.objectFilter.add(object.getClass().getName(), ctx);
			}
		}
	}

	public <T> void update(T object) {
		assertNotClosed();
		assertNotNull(object);
		PersistenceContext<T> ctx = createCtx(object);
		ctx.setObject(object);
		ctx.getObjectRef().lock();
		this.objectFilter.update(object.getClass().getName(), ctx);
	}

	public <T> void updateAll(List<T> objects) {
		assertNotClosed();
		assertNotNull(objects);
		if (!objects.isEmpty()) {
			for (T object : objects) {
				PersistenceContext<T> ctx = createCtx(object);
				ctx.setObject(object);
				ctx.getObjectRef().lock();
				this.objectFilter.update(object.getClass().getName(), ctx);
			}
		}
	}

	public <T> void remove(T object) {
		assertNotClosed();
		assertNotNull(object);
		PersistenceContext<T> ctx = createCtx(object);
		ctx.setObject(object);
		ctx.getObjectRef().lock();
		this.objectFilter.remove(object.getClass().getName(), ctx);
	}

	public <T> void removeAll(List<T> objects) {
		assertNotClosed();
		assertNotNull(objects);
		if (!objects.isEmpty()) {
			for (T object : objects) {
				PersistenceContext<T> ctx = createCtx(object);
				ctx.setObject(object);
				ctx.getObjectRef().lock();
				this.objectFilter.remove(object.getClass().getName(), ctx);
			}
		}
	}

	public <T> void removeById(ObjectRef objectRef) {
		assertNotClosed();
		assertIsIdRef(objectRef);
		PersistenceContext<T> ctx = createCtx(objectRef);
		ctx.getObjectRef().lock();
		this.objectFilter.remove(objectRef.getType(), ctx);
	}

	public <T> void removeAll(ObjectRef parentRef) {
		assertNotClosed();
		assertIsNotIdRef(parentRef);
		assertIsNotRootRef(parentRef);

		parentRef.lock();
		try {

			Set<String> keySet = queryKeySet(parentRef);
			for (String id : keySet) {

				ObjectRef childRef = parentRef.getChildIdRef(this.tx, id);
				PersistenceContext<T> ctx = createCtx(childRef);
				ctx.getObjectRef().lock();
				this.objectFilter.remove(childRef.getType(), ctx);
			}
		} finally {
			parentRef.unlock();
		}
	}

	public <T> T queryById(ObjectRef objectRef) {
		assertNotClosed();
		assertIsIdRef(objectRef);

		objectRef.lock();
		try {
			PersistenceContext<T> ctx = objectRef.<T> createPersistenceContext(this.tx);
			ctx.getObjectRef().lock();
			try {
				this.fileDao.performRead(ctx);
				return ctx.getObject();
			} finally {
				ctx.getObjectRef().unlock();
			}
		} finally {
			objectRef.unlock();
		}
	}

	public <T> List<T> queryAll(ObjectRef parentRef) {
		assertNotClosed();
		assertIsNotIdRef(parentRef);

		parentRef.lock();
		try {

			MetadataDao metadataDao = this.tx.getMetadataDao();
			Set<String> keySet = metadataDao.queryKeySet(parentRef);

			List<T> result = new ArrayList<>();
			for (String id : keySet) {

				ObjectRef childRef = parentRef.getChildIdRef(this.tx, id);
				PersistenceContext<T> childCtx = childRef.createPersistenceContext(this.tx);
				childCtx.getObjectRef().lock();
				try {
					this.fileDao.performRead(childCtx);
					assertObjectRead(childCtx);
					result.add(childCtx.getObject());
				} finally {
					childCtx.getObjectRef().unlock();
				}
			}

			return result;

		} finally {
			parentRef.unlock();
		}
	}

	public <T> Set<String> queryKeySet(ObjectRef parentRef) {
		assertNotClosed();
		assertIsNotIdRef(parentRef);

		parentRef.lock();
		try {
			MetadataDao metadataDao = this.tx.getMetadataDao();
			Set<String> keySet = metadataDao.queryKeySet(parentRef);
			return keySet;
		} finally {
			parentRef.unlock();
		}
	}

	public <T> long querySize(ObjectRef parentRef) {
		assertNotClosed();
		assertIsNotIdRef(parentRef);

		parentRef.lock();
		try {
			MetadataDao metadataDao = this.tx.getMetadataDao();
			long size = metadataDao.querySize(parentRef);
			return size;
		} finally {
			parentRef.unlock();
		}
	}

	private <T> PersistenceContext<T> createCtx(T object) {
		return this.ctxFactoryDelegator.<T> getCtxFactory(object.getClass()).createCtx(this.tx.getObjectRefCache(),
				object);
	}

	private <T> PersistenceContext<T> createCtx(ObjectRef objectRef) {
		String type = objectRef.getType();
		PersistenceContextFactory<T> ctxFactory = this.ctxFactoryDelegator.<T> getCtxFactory(type);
		return ctxFactory.createCtx(objectRef);
	}

	private void assertNotClosed() {
		if (!this.tx.isOpen())
			throw new IllegalStateException("Transaction has been closed and thus no operation can be performed!"); //$NON-NLS-1$
	}
}
