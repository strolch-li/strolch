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
package li.strolch.agent.impl;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.ActivityMap;
import li.strolch.agent.api.OrderMap;
import li.strolch.agent.api.ResourceMap;
import li.strolch.agent.api.StrolchRealm;
import li.strolch.model.Locator;
import li.strolch.privilege.model.Certificate;

final class SnapshotTransientTransaction extends TransientTransaction {

	private TransientRealm.Snapshot snapshot;
	private boolean refreshedStaleSnapshot;

	// AbstractTransaction reads configuration during construction, so initialize lazily there as well.
	private TransientRealm.Snapshot snapshot() {
		if (this.snapshot == null)
			this.snapshot = ((TransientRealm) getRealm()).snapshot();
		return this.snapshot;
	}

	@Override
	protected ResourceMap resourceMapForTransaction() {
		return snapshot().resources();
	}

	@Override
	protected OrderMap orderMapForTransaction() {
		return snapshot().orders();
	}

	@Override
	protected ActivityMap activityMapForTransaction() {
		return snapshot().activities();
	}

	@Override
	protected void refreshLockedElement(Locator locator) {
		TransientRealm realm = (TransientRealm) getRealm();
		synchronized (realm) {
			this.refreshedStaleSnapshot |= !realm.isCurrent(snapshot());
			realm.refresh(snapshot(), locator);
		}
	}

	@Override
	protected void validateBeforeCommit() {
		if (needsCommit() || snapshot().modified() || this.refreshedStaleSnapshot)
			((TransientRealm) getRealm()).validate(snapshot());
	}

	@Override
	protected void validateBeforeReadOnlyClose() {
		// A current-value lock refresh can mix revisions; do not report such a read as successful.
		if (this.refreshedStaleSnapshot)
			((TransientRealm) getRealm()).validate(snapshot());
	}

	SnapshotTransientTransaction(ComponentContainer container, StrolchRealm realm, Certificate certificate,
			String action, boolean readOnly) {
		super(container, realm, certificate, action, readOnly);
	}

	@Override
	protected void commit() throws Exception {
		((TransientRealm) getRealm()).publish(snapshot());
		super.commit();
	}
}
