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

import java.util.Set;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.StrolchRealm;
import li.strolch.persistence.api.AbstractTransaction;
import li.strolch.persistence.api.PersistenceHandler;
import li.strolch.privilege.model.Certificate;
import li.strolch.xmlpers.api.ModificationResult;
import li.strolch.xmlpers.api.PersistenceTransaction;
import li.strolch.xmlpers.api.TransactionResult;

public class XmlStrolchTransaction extends AbstractTransaction {

	private XmlPersistenceHandler persistenceHandler;
	private PersistenceTransaction tx;

	public XmlStrolchTransaction(ComponentContainer container, StrolchRealm realm, Certificate certificate,
			String action, PersistenceTransaction tx, XmlPersistenceHandler persistenceHandler) {
		super(container, realm, certificate, action);
		this.persistenceHandler = persistenceHandler;
		this.tx = tx;
	}

	PersistenceTransaction getTx() {
		return this.tx;
	}

	@Override
	protected void writeChanges() throws Exception {
		// do nothing
	}

	@Override
	protected void rollback() throws Exception {
		this.tx.autoCloseableRollback();
	}

	@Override
	protected void commit() throws Exception {
		if (!this.tx.hasTransactionResult()) {
			TransactionResult result = new TransactionResult();
			this.tx.setTransactionResult(result);
		}

		this.tx.autoCloseableCommit();

		TransactionResult txResult = this.tx.getTransactionResult();
		updateTxResult(txResult);
		txResult.clear();
	}

	private void updateTxResult(TransactionResult result) {
		Set<String> keys = result.getKeys();
		for (String key : keys) {
			ModificationResult modificationResult = result.getModificationResult(key);
			getTxResult().incCreated(modificationResult.getCreated().size());
			getTxResult().incUpdated(modificationResult.getUpdated().size());
			getTxResult().incDeleted(modificationResult.getDeleted().size());
		}
	}

	@Override
	public PersistenceHandler getPersistenceHandler() {
		return this.persistenceHandler;
	}
}
