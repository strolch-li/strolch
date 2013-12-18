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
package li.strolch.persistence.postgresql;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import li.strolch.model.StrolchElement;
import li.strolch.persistence.api.StrolchDao;
import li.strolch.persistence.api.StrolchTransaction;

public abstract class AbstractDao<T extends StrolchElement> implements StrolchDao<T> {

	protected AbstractDao(StrolchTransaction tx) {
		PostgreSqlStrolchTransaction strolchTx = (PostgreSqlStrolchTransaction) tx;
	}

	protected abstract String getClassType();

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
		return null;
	}

	@Override
	public Set<String> queryTypes() {
		return null;
	}

	@Override
	public T queryBy(String type, String id) {
		return null;
	}

	@Override
	public List<T> queryAll() {
		return null;
	}

	@Override
	public List<T> queryAll(String type) {
		return null;
	}

	@Override
	public void save(T object) {
	}

	@Override
	public void saveAll(List<T> objects) {
	}

	@Override
	public void update(T object) {
	}

	@Override
	public void updateAll(List<T> objects) {
	}

	@Override
	public void remove(T object) {
	}

	@Override
	public void removeAll(List<T> objects) {
	}

	@Override
	public void remove(String type, String id) {
	}
}
