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
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import li.strolch.model.StrolchElement;
import li.strolch.persistence.api.StrolchDao;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.xmlpers.api.PersistenceTransaction;
import li.strolch.xmlpers.objref.IdOfSubTypeRef;
import li.strolch.xmlpers.objref.SubTypeRef;
import li.strolch.xmlpers.objref.TypeRef;

public abstract class AbstractDao<T extends StrolchElement> implements StrolchDao<T> {

	protected PersistenceTransaction tx;

	protected AbstractDao(StrolchTransaction tx) {
		XmlStrolchTransaction strolchTx = (XmlStrolchTransaction) tx;
		this.tx = strolchTx.getTx();
	}

	protected abstract String getClassType();

	protected IdOfSubTypeRef getIdRef(String type, String id) {
		IdOfSubTypeRef idRef = this.tx.getObjectRefCache().getIdOfSubTypeRef(getClassType(), type, id);
		return idRef;
	}

	protected SubTypeRef getTypeRef(String type) {
		SubTypeRef typeRef = this.tx.getObjectRefCache().getSubTypeRef(getClassType(), type);
		return typeRef;
	}

	@Override
	public boolean hasElement(String type, String id) {
		IdOfSubTypeRef ref = getIdRef(type, id);
		return this.tx.getObjectDao().hasElement(ref);
	}

	@Override
	public long querySize() {
		long size = 0;
		Set<String> types = queryTypes();
		for (String type : types) {

			SubTypeRef subTypeRef = getTypeRef(type);
			size += this.tx.getMetadataDao().querySize(subTypeRef);
		}
		return size;
	}

	@Override
	public long querySize(String type) {
		SubTypeRef subTypeRef = getTypeRef(type);
		return this.tx.getMetadataDao().querySize(subTypeRef);
	}

	@Override
	public Set<String> queryKeySet() {
		Set<String> keys = new HashSet<>();
		Set<String> types = queryTypes();
		for (String type : types) {
			keys.addAll(queryKeySet(type));
		}
		return keys;
	}

	@Override
	public Set<String> queryKeySet(String type) {
		SubTypeRef typeRef = getTypeRef(type);
		Set<String> keys = this.tx.getMetadataDao().queryKeySet(typeRef);
		return keys;
	}

	@Override
	public Set<String> queryTypes() {
		TypeRef typeRef = this.tx.getObjectRefCache().getTypeRef(getClassType());
		Set<String> types = this.tx.getMetadataDao().queryTypeSet(typeRef);
		return types;
	}

	@Override
	public T queryBy(String type, String id) {
		T t = this.tx.getObjectDao().queryById(getIdRef(type, id));
		return t;
	}

	@Override
	public List<T> queryAll() {
		List<T> objects = new ArrayList<>();
		Set<String> types = queryTypes();
		for (String type : types) {
			List<T> objectsByType = this.tx.getObjectDao().queryAll(getTypeRef(type));
			objects.addAll(objectsByType);
		}

		return objects;
	}

	@Override
	public List<T> queryAll(String type) {
		List<T> objectsByType = this.tx.getObjectDao().queryAll(getTypeRef(type));
		return objectsByType;
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
		TypeRef typeRef = this.tx.getObjectRefCache().getTypeRef(getClassType());
		return this.tx.getObjectDao().removeAllBy(typeRef);
	}

	@Override
	public long removeAllBy(String type) {
		SubTypeRef typeRef = getTypeRef(type);
		return this.tx.getObjectDao().removeAllBy(typeRef);
	}
}
