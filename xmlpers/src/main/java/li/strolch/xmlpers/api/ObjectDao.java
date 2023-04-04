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

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.function.Predicate;

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
	private final PersistenceContextFactoryDelegator ctxFactoryDelegator;

	public ObjectDao(PersistenceTransaction tx, FileDao fileDao, ObjectFilter objectFilter) {
		this.tx = tx;
		this.fileDao = fileDao;
		this.objectFilter = objectFilter;
		this.ctxFactoryDelegator = this.tx.getManager().getCtxFactory();
	}

	public <T> void add(T object) {
		add(object, -1L);
	}

	public <T> void add(T object, long lastModified) {
		assertNotClosed();
		assertNotNull(object);
		PersistenceContext<T> ctx = createCtx(object, lastModified);
		this.tx.lock(ctx.getObjectRef().getParent(this.tx));
		this.tx.lock(ctx.getObjectRef());
		this.objectFilter.add(ctx.getObjectRef().getType(), ctx.getObjectRef(), ctx);
	}

	public <T> void addAll(List<T> objects) {
		addAll(objects, -1L);
	}

	public <T> void addAll(List<T> objects, long lastModified) {
		assertNotClosed();
		assertNotNull(objects);
		if (objects.isEmpty())
			return;

		for (T object : objects) {
			PersistenceContext<T> ctx = createCtx(object, lastModified);
			this.tx.lock(ctx.getObjectRef().getParent(this.tx));
			this.tx.lock(ctx.getObjectRef());
			this.objectFilter.add(ctx.getObjectRef().getType(), ctx.getObjectRef(), ctx);
		}
	}

	public <T> void update(T object) {
		update(object, -1L);
	}

	public <T> void update(T object, long lastModified) {
		assertNotClosed();
		assertNotNull(object);
		PersistenceContext<T> ctx = createCtx(object, lastModified);
		this.tx.lock(ctx.getObjectRef());
		this.objectFilter.update(ctx.getObjectRef().getType(), ctx.getObjectRef(), ctx);
	}

	public <T> void updateAll(List<T> objects) {
		updateAll(objects, -1L);
	}

	public <T> void updateAll(List<T> objects, long lastModified) {
		assertNotClosed();
		assertNotNull(objects);
		if (objects.isEmpty())
			return;

		for (T object : objects) {
			PersistenceContext<T> ctx = createCtx(object, lastModified);
			this.tx.lock(ctx.getObjectRef());
			this.objectFilter.update(ctx.getObjectRef().getType(), ctx.getObjectRef(), ctx);
		}
	}

	public <T> void remove(T object) {
		assertNotClosed();
		assertNotNull(object);
		PersistenceContext<T> ctx = createCtx(object, -1L);
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
			PersistenceContext<T> ctx = createCtx(object, -1L);
			this.tx.lock(ctx.getObjectRef().getParent(this.tx));
			this.tx.lock(ctx.getObjectRef());
			this.objectFilter.remove(ctx.getObjectRef().getType(), ctx.getObjectRef(), ctx);
		}
	}

	public <T> long removeAllBy(TypeRef typeRef, Predicate<File> predicate) {
		assertNotClosed();

		long removed = 0;

		this.tx.lock(typeRef);

		Set<String> types = this.tx.getMetadataDao().queryTypeSet(typeRef);
		for (String type : types) {
			ObjectRef childTypeRef = typeRef.getChildTypeRef(this.tx, type);
			this.tx.lock(childTypeRef);

			Set<String> ids = queryKeySet(childTypeRef, false, predicate);
			for (String id : ids) {

				ObjectRef idRef = childTypeRef.getChildIdRef(this.tx, id);

				PersistenceContext<T> ctx = createCtx(idRef, -1L);
				this.tx.lock(ctx.getObjectRef());
				this.objectFilter.remove(ctx.getObjectRef().getType(), ctx.getObjectRef(), ctx);
				removed++;
			}
		}

		return removed;
	}

	public <T> long removeAllBy(SubTypeRef subTypeRef, Predicate<File> predicate) {
		assertNotClosed();
		assertIsNotRootRef(subTypeRef);
		assertIsNotIdRef(subTypeRef);

		long removed = 0;

		this.tx.lock(subTypeRef);

		Set<String> ids = queryKeySet(subTypeRef, false, predicate);
		for (String id : ids) {

			ObjectRef idRef = subTypeRef.getChildIdRef(this.tx, id);

			PersistenceContext<T> ctx = createCtx(idRef, -1L);
			this.tx.lock(ctx.getObjectRef());
			this.objectFilter.remove(ctx.getObjectRef().getType(), ctx.getObjectRef(), ctx);
			removed++;
		}

		return removed;
	}

	public <T> void removeById(ObjectRef objectRef) {
		assertNotClosed();
		assertIsIdRef(objectRef);
		PersistenceContext<T> ctx = createCtx(objectRef, -1L);
		this.tx.lock(ctx.getObjectRef().getParent(this.tx));
		this.tx.lock(ctx.getObjectRef());
		this.objectFilter.remove(objectRef.getType(), ctx.getObjectRef(), ctx);
	}

	public <T> void removeAll(ObjectRef parentRef) {
		assertNotClosed();
		assertIsNotIdRef(parentRef);
		assertIsNotRootRef(parentRef);

		this.tx.lock(parentRef);

		Set<String> keySet = queryKeySet(parentRef, false, file -> true);
		for (String id : keySet) {

			ObjectRef childRef = parentRef.getChildIdRef(this.tx, id);
			PersistenceContext<T> ctx = createCtx(childRef, -1L);
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

	public <T> List<T> queryAll(ObjectRef parentRef, Predicate<File> predicate) {
		return queryAll(parentRef, false, predicate, Integer.MAX_VALUE);
	}

	public <T> List<T> queryAll(ObjectRef parentRef, boolean reverse, Predicate<File> predicate, int maxSize) {
		assertNotClosed();
		assertIsNotIdRef(parentRef);

		this.tx.lock(parentRef);

		MetadataDao metadataDao = this.tx.getMetadataDao();
		Set<String> keySet = metadataDao.queryKeySet(parentRef, reverse, predicate);

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

	private Set<String> queryKeySet(ObjectRef parentRef, boolean reverse, Predicate<File> predicate) {
		assertNotClosed();
		assertIsNotIdRef(parentRef);

		this.tx.lock(parentRef);
		MetadataDao metadataDao = this.tx.getMetadataDao();
		return metadataDao.queryKeySet(parentRef, reverse, predicate);
	}

	public long querySize(ObjectRef parentRef, Predicate<File> predicate) {
		assertNotClosed();
		assertIsNotIdRef(parentRef);

		this.tx.lock(parentRef);
		MetadataDao metadataDao = this.tx.getMetadataDao();
		return metadataDao.querySize(parentRef, predicate);
	}

	public <T> PersistenceContext<T> createCtx(T object, long lastModified) {
		PersistenceContext<T> ctx = this.ctxFactoryDelegator.<T>getCtxFactory(object.getClass())
				.createCtx(this.tx.getManager().getObjectRefCache(), object);
		ctx.setLastModified(lastModified);
		return ctx;
	}

	public <T> PersistenceContext<T> createCtx(ObjectRef objectRef, long lastModified) {
		String type = objectRef.getType();
		PersistenceContextFactory<T> ctxFactory = this.ctxFactoryDelegator.<T>getCtxFactory(type);
		PersistenceContext<T> ctx = ctxFactory.createCtx(objectRef);
		ctx.setLastModified(lastModified);
		return ctx;
	}

	private void assertNotClosed() {
		if (!this.tx.isOpen())
			throw new IllegalStateException(
					"Transaction has been closed and thus no operation can be performed!"); //$NON-NLS-1$
	}
}
