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
package li.strolch.xmlpers.api;

import static li.strolch.xmlpers.util.AssertionUtil.*;

import java.util.*;

import li.strolch.utils.objectfilter.ObjectFilter;
import li.strolch.xmlpers.objref.ObjectRef;
import li.strolch.xmlpers.objref.SubTypeRef;
import li.strolch.xmlpers.objref.TypeRef;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
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
		this.ctxFactoryDelegator = this.tx.getManager().getCtxFactory();
	}

	public <T> void add(T object) {
		assertNotClosed();
		assertNotNull(object);
		PersistenceContext<T> ctx = createCtx(object);
		this.tx.lock(ctx.getObjectRef().getParent(this.tx));
		this.tx.lock(ctx.getObjectRef());
		this.objectFilter.add(ctx.getObjectRef().getType(), ctx.getObjectRef(), ctx);
	}

	public <T> void addAll(List<T> objects) {
		assertNotClosed();
		assertNotNull(objects);
		if (objects.isEmpty())
			return;

		for (T object : objects) {
			PersistenceContext<T> ctx = createCtx(object);
			this.tx.lock(ctx.getObjectRef().getParent(this.tx));
			this.tx.lock(ctx.getObjectRef());
			this.objectFilter.add(ctx.getObjectRef().getType(), ctx.getObjectRef(), ctx);
		}
	}

	public <T> void update(T object) {
		assertNotClosed();
		assertNotNull(object);
		PersistenceContext<T> ctx = createCtx(object);
		this.tx.lock(ctx.getObjectRef());
		this.objectFilter.update(ctx.getObjectRef().getType(), ctx.getObjectRef(), ctx);
	}

	public <T> void updateAll(List<T> objects) {
		assertNotClosed();
		assertNotNull(objects);
		if (objects.isEmpty())
			return;

		for (T object : objects) {
			PersistenceContext<T> ctx = createCtx(object);
			this.tx.lock(ctx.getObjectRef());
			this.objectFilter.update(ctx.getObjectRef().getType(), ctx.getObjectRef(), ctx);
		}
	}

	public <T> void remove(T object) {
		assertNotClosed();
		assertNotNull(object);
		PersistenceContext<T> ctx = createCtx(object);
		this.tx.lock(ctx.getObjectRef().getParent(this.tx));
		this.tx.lock(ctx.getObjectRef());
		this.objectFilter.remove(ctx.getObjectRef().getType(), ctx.getObjectRef(), ctx);
	}

	public <T> void removeAll(List<T> objects) {
		assertNotClosed();
		assertNotNull(objects);
		if (objects.isEmpty())
			return;

		for (T object : objects) {
			PersistenceContext<T> ctx = createCtx(object);
			this.tx.lock(ctx.getObjectRef().getParent(this.tx));
			this.tx.lock(ctx.getObjectRef());
			this.objectFilter.remove(ctx.getObjectRef().getType(), ctx.getObjectRef(), ctx);
		}
	}

	public <T> long removeAllBy(TypeRef typeRef) {
		assertNotClosed();

		long removed = 0;

		this.tx.lock(typeRef);

		Set<String> types = this.tx.getMetadataDao().queryTypeSet(typeRef);
		for (String type : types) {
			ObjectRef childTypeRef = typeRef.getChildTypeRef(this.tx, type);
			this.tx.lock(childTypeRef);

			Set<String> ids = queryKeySet(childTypeRef, false);
			for (String id : ids) {

				ObjectRef idRef = childTypeRef.getChildIdRef(this.tx, id);

				PersistenceContext<T> ctx = createCtx(idRef);
				this.tx.lock(ctx.getObjectRef());
				this.objectFilter.remove(ctx.getObjectRef().getType(), ctx.getObjectRef(), ctx);
				removed++;
			}
		}

		return removed;
	}

	public <T> long removeAllBy(SubTypeRef subTypeRef) {
		assertNotClosed();
		assertIsNotRootRef(subTypeRef);
		assertIsNotIdRef(subTypeRef);

		long removed = 0;

		this.tx.lock(subTypeRef);

		Set<String> ids = queryKeySet(subTypeRef, false);
		for (String id : ids) {

			ObjectRef idRef = subTypeRef.getChildIdRef(this.tx, id);

			PersistenceContext<T> ctx = createCtx(idRef);
			this.tx.lock(ctx.getObjectRef());
			this.objectFilter.remove(ctx.getObjectRef().getType(), ctx.getObjectRef(), ctx);
			removed++;
		}

		return removed;
	}

	public <T> void removeById(ObjectRef objectRef) {
		assertNotClosed();
		assertIsIdRef(objectRef);
		PersistenceContext<T> ctx = createCtx(objectRef);
		this.tx.lock(ctx.getObjectRef().getParent(this.tx));
		this.tx.lock(ctx.getObjectRef());
		this.objectFilter.remove(objectRef.getType(), ctx.getObjectRef(), ctx);
	}

	public <T> void removeAll(ObjectRef parentRef) {
		assertNotClosed();
		assertIsNotIdRef(parentRef);
		assertIsNotRootRef(parentRef);

		this.tx.lock(parentRef);

		Set<String> keySet = queryKeySet(parentRef, false);
		for (String id : keySet) {

			ObjectRef childRef = parentRef.getChildIdRef(this.tx, id);
			PersistenceContext<T> ctx = createCtx(childRef);
			this.tx.lock(ctx.getObjectRef());
			this.objectFilter.remove(childRef.getType(), ctx.getObjectRef(), ctx);
		}
	}

	public <T> boolean hasElement(ObjectRef objectRef) {
		assertNotClosed();
		assertIsIdRef(objectRef);

		this.tx.lock(objectRef);
		PersistenceContext<T> ctx = objectRef.<T>createPersistenceContext(this.tx);
		return this.fileDao.exists(ctx);
	}

	public <T> T queryById(ObjectRef objectRef) {
		assertNotClosed();
		assertIsIdRef(objectRef);

		this.tx.lock(objectRef);
		PersistenceContext<T> ctx = objectRef.<T>createPersistenceContext(this.tx);
		this.fileDao.performRead(ctx);
		return ctx.getObject();
	}

	public <T> List<T> queryAll(ObjectRef parentRef) {
		return queryAll(parentRef, Integer.MAX_VALUE, false);
	}

	public <T> List<T> queryAll(ObjectRef parentRef, int maxSize, boolean reverse) {
		assertNotClosed();
		assertIsNotIdRef(parentRef);

		this.tx.lock(parentRef);

		MetadataDao metadataDao = this.tx.getMetadataDao();
		Set<String> keySet = metadataDao.queryKeySet(parentRef, reverse);

		int i = 0;
		List<T> result = new ArrayList<>();
		for (String id : keySet) {

			ObjectRef childRef = parentRef.getChildIdRef(this.tx, id);
			PersistenceContext<T> childCtx = childRef.createPersistenceContext(this.tx);
			this.tx.lock(childCtx.getObjectRef());
			this.fileDao.performRead(childCtx);
			assertObjectRead(childCtx);
			result.add(childCtx.getObject());

			if (maxSize != Integer.MAX_VALUE && i >= maxSize)
				break;
		}

		return result;
	}

	private Set<String> queryKeySet(ObjectRef parentRef, boolean reverse) {
		assertNotClosed();
		assertIsNotIdRef(parentRef);

		this.tx.lock(parentRef);
		MetadataDao metadataDao = this.tx.getMetadataDao();
		return metadataDao.queryKeySet(parentRef, reverse);
	}

	public long querySize(ObjectRef parentRef) {
		assertNotClosed();
		assertIsNotIdRef(parentRef);

		this.tx.lock(parentRef);
		MetadataDao metadataDao = this.tx.getMetadataDao();
		return metadataDao.querySize(parentRef);
	}

	public <T> PersistenceContext<T> createCtx(T object) {
		return this.ctxFactoryDelegator.<T>getCtxFactory(object.getClass())
				.createCtx(this.tx.getManager().getObjectRefCache(), object);
	}

	public <T> PersistenceContext<T> createCtx(ObjectRef objectRef) {
		String type = objectRef.getType();
		PersistenceContextFactory<T> ctxFactory = this.ctxFactoryDelegator.<T>getCtxFactory(type);
		return ctxFactory.createCtx(objectRef);
	}

	private void assertNotClosed() {
		if (!this.tx.isOpen())
			throw new IllegalStateException(
					"Transaction has been closed and thus no operation can be performed!"); //$NON-NLS-1$
	}
}
