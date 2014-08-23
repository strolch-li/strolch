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

import li.strolch.agent.api.StrolchRealm;
import li.strolch.model.StrolchRootElement;
import li.strolch.persistence.api.AbstractTransaction;
import li.strolch.persistence.api.PersistenceHandler;
import ch.eitchnet.privilege.model.Certificate;
import ch.eitchnet.xmlpers.api.ModificationResult;
import ch.eitchnet.xmlpers.api.PersistenceContext;
import ch.eitchnet.xmlpers.api.PersistenceTransaction;
import ch.eitchnet.xmlpers.api.TransactionResult;

public class XmlStrolchTransaction extends AbstractTransaction {

	private XmlPersistenceHandler persistenceHandler;
	private PersistenceTransaction tx;

	public XmlStrolchTransaction(StrolchRealm realm, Certificate certificate, String action, PersistenceTransaction tx,
			XmlPersistenceHandler persistenceHandler) {
		super(realm, certificate, action);
		this.persistenceHandler = persistenceHandler;
		this.tx = tx;
	}

	PersistenceTransaction getTx() {
		return this.tx;
	}

	@Override
	protected void writeChanges(li.strolch.persistence.api.TransactionResult txResult) throws Exception {
		TransactionResult result = new TransactionResult();
		this.tx.setTransactionResult(result);
		this.tx.autoCloseableCommit();
		Set<String> keys = result.getKeys();
		for (String key : keys) {
			ModificationResult modificationResult = result.getModificationResult(key);

			List<StrolchRootElement> created = new ArrayList<>();
			List<StrolchRootElement> updated = new ArrayList<>();
			List<StrolchRootElement> deleted = new ArrayList<>();

			List<Object> createdCtx = modificationResult.getCreated();
			for (Object object : createdCtx) {
				@SuppressWarnings("unchecked")
				PersistenceContext<StrolchRootElement> ctx = (PersistenceContext<StrolchRootElement>) object;
				if (ctx.getObject() != null)
					created.add(ctx.getObject());
			}
			List<Object> updatedCtx = modificationResult.getUpdated();
			for (Object object : updatedCtx) {
				@SuppressWarnings("unchecked")
				PersistenceContext<StrolchRootElement> ctx = (PersistenceContext<StrolchRootElement>) object;
				if (ctx.getObject() != null)
					updated.add(ctx.getObject());
			}
			List<Object> deletedCtx = modificationResult.getDeleted();
			for (Object object : deletedCtx) {
				@SuppressWarnings("unchecked")
				PersistenceContext<StrolchRootElement> ctx = (PersistenceContext<StrolchRootElement>) object;
				if (ctx.getObject() != null)
					deleted.add(ctx.getObject());
			}

			li.strolch.persistence.api.ModificationResult mr = new li.strolch.persistence.api.ModificationResult(key,
					created, updated, deleted);

			txResult.addModificationResult(mr);
		}
	}

	@Override
	protected void rollback(li.strolch.persistence.api.TransactionResult txResult) throws Exception {
		this.tx.autoCloseableRollback();
	}

	@Override
	protected void commit() throws Exception {
		// no-op
	}

	@Override
	public PersistenceHandler getPersistenceHandler() {
		return this.persistenceHandler;
	}
}
