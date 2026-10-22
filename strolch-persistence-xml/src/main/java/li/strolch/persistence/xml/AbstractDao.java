/*
 * Copyright (c) 2013-2025 Robert von Burg <eitch@eitchnet.ch>
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

import li.strolch.model.StrolchRootElement;
import li.strolch.persistence.api.StrolchDao;
import li.strolch.persistence.api.StrolchPersistenceException;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.xmlpers.api.PersistenceTransaction;
import li.strolch.xmlpers.objref.ObjectRef;
import li.strolch.xmlpers.objref.SubTypeRef;
import li.strolch.xmlpers.objref.TypeRef;

import java.util.*;
import java.util.stream.Collectors;

public abstract class AbstractDao<T extends StrolchRootElement> implements StrolchDao<T> {

	protected final PersistenceTransaction tx;

	protected AbstractDao(StrolchTransaction tx) {
		XmlStrolchTransaction strolchTx = (XmlStrolchTransaction) tx;
		this.tx = strolchTx.getTx();
	}

	@Override
	public boolean supportsPaging() {
		return true;
	}

	protected abstract String getClassType();

	protected SubTypeRef getTypeRef(String type) {
		return this.tx.getManager().getObjectRefCache().getSubTypeRef(getClassType(), type);
	}

	@Override
	public long querySize() {
		Set<String> types = queryTypes();
		if (types.isEmpty())
			return 0L;
		if (types.size() == 1) {
			SubTypeRef subTypeRef = getTypeRef(types.iterator().next());
			return this.tx.getMetadataDao().querySize(subTypeRef, file -> true);
		}
		return types.parallelStream().mapToLong(type -> {
			SubTypeRef subTypeRef = getTypeRef(type);
			return this.tx.getMetadataDao().querySize(subTypeRef, file -> true);
		}).sum();
	}

	@Override
	public long querySize(String... types) {
		if (types.length == 0)
			return querySize();

		if (types.length == 1) {
			SubTypeRef subTypeRef = getTypeRef(types[0]);
			return this.tx.getMetadataDao().querySize(subTypeRef, file -> true);
		}

		return Arrays.stream(types).parallel().mapToLong(type -> {
			SubTypeRef subTypeRef = getTypeRef(type);
			return this.tx.getMetadataDao().querySize(subTypeRef, file -> true);
		}).sum();
	}

	@Override
	public long querySize(T element) {
		return querySize(element.getType(), element.getId());
	}

	@Override
	public long querySize(String type, String id) {
		return this.tx.getObjectDao().hasElement(getTypeRef(type).getChildIdRef(this.tx, id)) ? 1L : 0L;
	}

	@Override
	public Set<String> queryTypes() {
		TypeRef typeRef = this.tx.getManager().getObjectRefCache().getTypeRef(getClassType());
		return this.tx.getMetadataDao().queryTypeSet(typeRef);
	}

	@Override
	public List<T> queryAll() {
		Set<String> types = queryTypes();
		if (types.isEmpty())
			return new ArrayList<>();
		if (types.size() == 1) {
			String type = types.iterator().next();
			return new ArrayList<>(this.tx.getObjectDao().queryAll(getTypeRef(type), file -> true));
		}

		return types.parallelStream()
				.flatMap(type -> this.tx.getObjectDao().<T>queryAll(getTypeRef(type), file -> true).stream())
				.collect(Collectors.toCollection(ArrayList::new));
	}

	@Override
	public List<T> queryAll(String... types) {
		if (types.length == 0)
			return queryAll();

		if (types.length == 1) {
			return new ArrayList<>(this.tx.getObjectDao().queryAll(getTypeRef(types[0]), file -> true));
		}

		return Arrays.stream(types)
				.parallel()
				.flatMap(type -> this.tx.getObjectDao().<T>queryAll(getTypeRef(type), file -> true).stream())
				.collect(Collectors.toCollection(ArrayList::new));
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
		if (limit <= 0)
			return Collections.emptyList();

		Set<String> types = queryTypes();
		if (types.isEmpty())
			return Collections.emptyList();

		if (types.size() == 1) {
			String type = types.iterator().next();
			return this.tx.getObjectDao().queryAll(getTypeRef(type), false, file -> true, limit, offset);
		}

		List<ObjectRef> childRefs = new ArrayList<>();
		for (String type : types) {
			SubTypeRef subTypeRef = getTypeRef(type);
			Set<String> keySet = this.tx.getMetadataDao().queryKeySet(subTypeRef, file -> true);
			for (String id : keySet) {
				childRefs.add(subTypeRef.getChildIdRef(this.tx, id));
			}
		}

		List<ObjectRef> selectedRefs = childRefs.stream().skip(offset).limit(limit).toList();
		return this.tx.getObjectDao().queryAll(selectedRefs);
	}

	@Override
	public List<T> queryAll(long limit, long offset, String... types) throws StrolchPersistenceException {
		if (types.length == 0)
			return queryAll(limit, offset);

		if (limit <= 0)
			return Collections.emptyList();

		if (types.length == 1) {
			return this.tx.getObjectDao().queryAll(getTypeRef(types[0]), false, file -> true, limit, offset);
		}

		List<ObjectRef> childRefs = new ArrayList<>();
		for (String type : types) {
			SubTypeRef subTypeRef = getTypeRef(type);
			Set<String> keySet = this.tx.getMetadataDao().queryKeySet(subTypeRef, file -> true);
			for (String id : keySet) {
				childRefs.add(subTypeRef.getChildIdRef(this.tx, id));
			}
		}

		List<ObjectRef> selectedRefs = childRefs.stream().skip(offset).limit(limit).toList();
		return this.tx.getObjectDao().queryAll(selectedRefs);
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
