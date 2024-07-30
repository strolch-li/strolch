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
package li.strolch.persistence.xml;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import li.strolch.model.StrolchRootElement;
import li.strolch.persistence.api.StrolchDao;
import li.strolch.persistence.api.StrolchPersistenceException;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.xmlpers.api.PersistenceTransaction;
import li.strolch.xmlpers.objref.SubTypeRef;
import li.strolch.xmlpers.objref.TypeRef;

public abstract class AbstractDao<T extends StrolchRootElement> implements StrolchDao<T> {

	protected final PersistenceTransaction tx;

	protected AbstractDao(StrolchTransaction tx) {
		XmlStrolchTransaction strolchTx = (XmlStrolchTransaction) tx;
		this.tx = strolchTx.getTx();
	}

	@Override
	public boolean supportsPaging() {
		return false;
	}

	protected abstract String getClassType();

	protected SubTypeRef getTypeRef(String type) {
		return this.tx.getManager().getObjectRefCache().getSubTypeRef(getClassType(), type);
	}

	@Override
	public long querySize() {
		long size = 0;
		Set<String> types = queryTypes();
		for (String type : types) {
			SubTypeRef subTypeRef = getTypeRef(type);
			size += this.tx.getMetadataDao().querySize(subTypeRef, file -> true);
		}
		return size;
	}

	@Override
	public long querySize(String... types) {

		if (types.length == 0)
			return querySize();

		if (types.length == 1) {
			SubTypeRef subTypeRef = getTypeRef(types[0]);
			return this.tx.getMetadataDao().querySize(subTypeRef, file -> true);
		}

		long size = 0;
		for (String type : types) {
			SubTypeRef subTypeRef = getTypeRef(type);
			size += this.tx.getMetadataDao().querySize(subTypeRef, file -> true);
		}

		return size;
	}

	@Override
	public Set<String> queryTypes() {
		TypeRef typeRef = this.tx.getManager().getObjectRefCache().getTypeRef(getClassType());
		return this.tx.getMetadataDao().queryTypeSet(typeRef);
	}

	@Override
	public List<T> queryAll() {
		List<T> objects = new ArrayList<>();
		Set<String> types = queryTypes();
		for (String type : types) {
			List<T> objectsByType = this.tx.getObjectDao().queryAll(getTypeRef(type), file -> true);
			objects.addAll(objectsByType);
		}

		return objects;
	}

	@Override
	public List<T> queryAll(String... types) {
		if (types.length == 0)
			return queryAll();

		if (types.length == 1) {
			return this.tx.getObjectDao().queryAll(getTypeRef(types[0]), file -> true);
		}

		List<T> objects = new ArrayList<>();
		for (String type : types) {
			objects.addAll(this.tx.getObjectDao().queryAll(getTypeRef(type), file -> true));
		}

		return objects;
	}

	@Override
	public void save(T object) {
		this.tx.getObjectDao().add(object);
	}

	@Override
	public void saveAll(List<T> objects) {
		this.tx.getObjectDao().addAll(objects);
	}

	@Override
	public void update(T object) {
		this.tx.getObjectDao().update(object);
	}

	@Override
	public void updateAll(List<T> objects) {
		this.tx.getObjectDao().updateAll(objects);
	}

	@Override
	public void remove(T object) {
		this.tx.getObjectDao().remove(object);
	}

	@Override
	public void removeAll(List<T> objects) {
		this.tx.getObjectDao().removeAll(objects);
	}

	@Override
	public long removeAll() {
		TypeRef typeRef = this.tx.getManager().getObjectRefCache().getTypeRef(getClassType());
		return this.tx.getObjectDao().removeAllBy(typeRef, file -> true);
	}

	@Override
	public long removeAllBy(String type) {
		SubTypeRef typeRef = getTypeRef(type);
		return this.tx.getObjectDao().removeAllBy(typeRef, file -> true);
	}

	@Override
	public List<T> queryAll(long limit, long offset) throws StrolchPersistenceException {
		throw new UnsupportedOperationException("Paging not supported! Check first with supportsPaging()");
	}

	@Override
	public List<T> queryAll(long limit, long offset, String... types) throws StrolchPersistenceException {
		throw new UnsupportedOperationException("Paging not supported! Check first with supportsPaging()");
	}

	@Override
	public void removeVersion(T element) throws StrolchPersistenceException {
		throw new UnsupportedOperationException("Versioning is not supported!");
	}

	@Override
	public T queryBy(String type, String id, int version) {
		throw new UnsupportedOperationException("Versioning is not supported!");
	}

	@Override
	public List<T> queryVersionsFor(String type, String id) {
		throw new UnsupportedOperationException("Versioning is not supported!");
	}

	@Override
	public long queryVersionsSizeFor(String type, String id) {
		throw new UnsupportedOperationException("Versioning is not supported!");
	}

	@Override
	public int queryLatestVersionFor(String type, String id) throws StrolchPersistenceException {
		throw new UnsupportedOperationException("Versioning is not supported!");
	}
}
