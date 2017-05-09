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
package li.strolch.persistence.inmemory;

import li.strolch.agent.api.StrolchRealm;
import li.strolch.persistence.api.AbstractTransaction;
import li.strolch.persistence.api.PersistenceHandler;
import li.strolch.persistence.api.TransactionState;
import li.strolch.privilege.model.Certificate;
import li.strolch.runtime.privilege.PrivilegeHandler;

public class InMemoryTransaction extends AbstractTransaction {

	private InMemoryPersistence persistenceHandler;

	public InMemoryTransaction(PrivilegeHandler privilegeHandler, StrolchRealm realm, Certificate certificate,
			String action, InMemoryPersistence persistenceHandler) {
		super(privilegeHandler, realm, certificate, action);
		this.persistenceHandler = persistenceHandler;
	}

	@Override
	protected void writeChanges() throws Exception {
		// do nothing
	}

	@Override
	protected void rollback() throws Exception {
		getTxResult().setState(TransactionState.ROLLED_BACK);
	}

	@Override
	protected void commit() throws Exception {
		getTxResult().setState(TransactionState.COMMITTED);
	}

	@Override
	public PersistenceHandler getPersistenceHandler() {
		return this.persistenceHandler;
	}
}
