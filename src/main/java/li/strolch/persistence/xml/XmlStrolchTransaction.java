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

import li.strolch.agent.api.StrolchRealm;
import li.strolch.persistence.api.AbstractTransaction;
import li.strolch.persistence.api.PersistenceHandler;
import ch.eitchnet.xmlpers.api.ModificationResult;
import ch.eitchnet.xmlpers.api.PersistenceTransaction;
import ch.eitchnet.xmlpers.api.TransactionResult;

public class XmlStrolchTransaction extends AbstractTransaction {

	private XmlPersistenceHandler persistenceHandler;
	private PersistenceTransaction tx;

	public XmlStrolchTransaction(StrolchRealm realm, PersistenceTransaction tx, XmlPersistenceHandler persistenceHandler) {
		super(realm);
		this.persistenceHandler = persistenceHandler;
		this.tx = tx;
	}

	PersistenceTransaction getTx() {
		return this.tx;
	}

	@Override
	protected void commit(li.strolch.persistence.api.TransactionResult txResult) throws Exception {
		TransactionResult result = new TransactionResult();
		this.tx.setTransactionResult(result);
		this.tx.autoCloseableCommit();
		Set<String> keys = result.getKeys();
		for (String key : keys) {
			ModificationResult modificationResult = result.getModificationResult(key);
			li.strolch.persistence.api.ModificationResult mr = new li.strolch.persistence.api.ModificationResult(key,
					modificationResult.getCreated(), modificationResult.getUpdated(), modificationResult.getDeleted());
			txResult.addModificationResult(mr);
		}
	}

	@Override
	protected void rollback(li.strolch.persistence.api.TransactionResult txResult) throws Exception {
		this.tx.autoCloseableRollback();
	}

	@Override
	public PersistenceHandler getPersistenceHandler() {
		return this.persistenceHandler;
	}
}
