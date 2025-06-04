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

import li.strolch.agent.api.AuditTrail;
import li.strolch.model.audit.Audit;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.utils.collections.DateRange;
import li.strolch.utils.dbc.DBC;
import org.eclipse.store.storage.types.StorageManager;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;

public class EclipseStoreAuditTrail implements AuditTrail {

	private static final Logger logger = LoggerFactory.getLogger(EclipseStoreAuditTrail.class);

	private final String realm;
	private final StorageManager storageManager;
	private EclipseStoreAuditRoot root;

	public EclipseStoreAuditTrail(String realm, StorageManager storageManager) {
		this.realm = realm;
		DBC.PRE.assertNotNull("storageManager must be set!", storageManager);
		this.storageManager = storageManager;
	}

	public StorageManager getStorageManager() {
		return this.storageManager;
	}

	public void initStorageManager() {
		if (this.root == null)
			this.root = new EclipseStoreAuditRoot();
		this.storageManager.setRoot(this.root);
	}

	public void storeRoot() {
		this.storageManager.storeRoot();
	}

	public void start() {
		this.storageManager.start();
		this.root = (EclipseStoreAuditRoot) this.storageManager.root();
	}

	public void stop() {
		if (this.storageManager.isRunning()) {
			try {
				this.storageManager.storeRoot();
			} catch (Exception e) {
				logger.error("Failed to store root for Audits on realm {}!", this.realm, e);
			}
		}
	}

	public void destroy() {
		this.storageManager.shutdown();
	}

	@Override
	public boolean isEnabled() {
		return true;
	}

	@Override
	public long querySize(StrolchTransaction tx) {
		return this.root.size();
	}

	@Override
	public long querySize(StrolchTransaction tx, DateRange dateRange) {
		return this.root.size(dateRange);
	}

	@Override
	public List<Audit> getAllElements(StrolchTransaction tx, DateRange dateRange) {
		return this.root.audits(dateRange).toList();
	}

	@Override
	public List<Audit> getAllElements(StrolchTransaction tx, String type, DateRange dateRange) {
		return this.root.audits(dateRange).filter(audit -> audit.getElementType().equals(type)).toList();
	}

	@Override
	public void add(StrolchTransaction tx, Audit audit) {
		this.root.addAudit(this.storageManager, audit);
	}

	@Override
	public void addAll(StrolchTransaction tx, List<Audit> audits) {
		this.root.addAudits(this.storageManager, audits);
	}
}
