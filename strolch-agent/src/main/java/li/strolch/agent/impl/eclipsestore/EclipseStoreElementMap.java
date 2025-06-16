/*
 * Copyright (c) 2025 Robert von Burg <eitch@eitchnet.ch>
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

package li.strolch.agent.impl.eclipsestore;

import li.strolch.agent.impl.BaseElementMap;
import li.strolch.agent.impl.DataStoreMode;
import li.strolch.model.StrolchRootElement;
import li.strolch.persistence.api.StrolchTransaction;
import org.eclipse.store.storage.types.StorageManager;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;
import java.util.Set;
import java.util.stream.Stream;

public abstract class EclipseStoreElementMap<T extends StrolchRootElement> extends BaseElementMap<T> {

	protected static final Logger logger = LoggerFactory.getLogger(EclipseStoreElementMap.class);

	private final String realm;
	private final String objectType;
	private final StorageManager storageManager;
	private EclipseStoreElementRoot<T> root;

	public EclipseStoreElementMap(String realm, String objectType, StorageManager storageManager) {
		this.realm = realm;
		this.objectType = objectType;
		this.storageManager = storageManager;
	}

	@Override
	public DataStoreMode getDataStoreMode() {
		return DataStoreMode.CACHED;
	}

	public StorageManager getStorageManager() {
		return this.storageManager;
	}

	public void initStorageManager() {
		if (this.root == null)
			this.root = new EclipseStoreElementRoot<>();
		this.storageManager.setRoot(this.root);
	}

	public void storeRoot() {
		this.storageManager.storeRoot();
	}

	public void start() {
		this.storageManager.start();
		//noinspection unchecked
		this.root = (EclipseStoreElementRoot<T>) this.storageManager.root();
	}

	public void stop() {
		if (this.storageManager.isRunning()) {
			try {
				this.storageManager.storeRoot();
			} catch (Exception e) {
				logger.error("Failed to store root for {} on realm {}!", this.objectType, this.realm, e);
			}
		}
	}

	public void destroy() {
		if (this.storageManager != null) {
			try {
				this.storageManager.shutdown();
			} catch (Exception e) {
				logger.error("Failed to shutdown storage manage for {} on realm {}!", this.objectType, this.realm, e);
			}
		}
	}

	@Override
	public synchronized boolean hasType(StrolchTransaction tx, String type) {
		return this.root.hasType(type);
	}

	@Override
	public synchronized boolean hasElement(StrolchTransaction tx, String type, String id) {
		return this.root.hasElement(type, id);
	}

	@Override
	public synchronized long querySize(StrolchTransaction tx) {
		return this.root.size();
	}

	@Override
	public synchronized long querySize(StrolchTransaction tx, String type) {
		return this.root.size(type);
	}

	@Override
	protected T _getBy(String type, String id) {
		return this.root.getBy(type, id);
	}

	@Override
	public synchronized List<T> getElementsBy(StrolchTransaction tx, String type) {
		return this.root.getByType(type);
	}

	@Override
	public synchronized Stream<T> stream(StrolchTransaction tx, String... types) {
		return this.root.stream(types);
	}

	@Override
	public synchronized Set<String> getTypes(StrolchTransaction tx) {
		return this.root.getTypes();
	}

	@Override
	public synchronized Set<String> getAllKeys(StrolchTransaction tx) {
		return this.root.getAllKeys();
	}

	@Override
	public synchronized Set<String> getKeysBy(StrolchTransaction tx, String type) {
		return this.root.getKeysBy(type);
	}

	@Override
	protected void internalInsert(T element) {
		this.root.add(storageManager, element);
	}

	protected void internalUpdate(T element) {
		this.root.update(storageManager, element);
	}

	@Override
	public synchronized void remove(StrolchTransaction tx, T element) {
		this.root.remove(storageManager, element);
	}

	@Override
	public synchronized long removeAll(StrolchTransaction tx) {
		return this.root.removeAll(storageManager);
	}

	@Override
	public synchronized long removeAllBy(StrolchTransaction tx, String type) {
		return this.root.removeAllByType(storageManager, type);
	}
}
