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

import li.strolch.persistence.api.StrolchPersistenceException;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.persistence.api.TransactionCloseStrategy;
import li.strolch.runtime.observer.ObserverHandler;

public class PostgreSqlStrolchTransaction implements StrolchTransaction {

	private ObserverHandler observerHandler;
	private boolean suppressUpdates;
	private TransactionCloseStrategy closeStrategy;
//	private TransactionResult txResult;

	public PostgreSqlStrolchTransaction(/*PersistenceTransaction tx*/) {
		this.suppressUpdates = false;
//		 this.tx = tx;
		this.closeStrategy = TransactionCloseStrategy.COMMIT;
	}

	/**
	 * @param observerHandler
	 *            the observerHandler to set
	 */
	public void setObserverHandler(ObserverHandler observerHandler) {
		this.observerHandler = observerHandler;
	}

	/**
	 * @param suppressUpdates
	 *            the suppressUpdates to set
	 */
	public void setSuppressUpdates(boolean suppressUpdates) {
		this.suppressUpdates = suppressUpdates;
	}

	/**
	 * @return the suppressUpdates
	 */
	public boolean isSuppressUpdates() {
		return this.suppressUpdates;
	}

	@Override
	public void setCloseStrategy(TransactionCloseStrategy closeStrategy) {
		this.closeStrategy = closeStrategy;
	}

	@Override
	public void autoCloseableCommit() {

		if (!this.suppressUpdates && this.observerHandler != null) {
//			this.txResult = new TransactionResult();
//			this.tx.setTransactionResult(this.txResult);
		}

//		this.tx.autoCloseableCommit();

		if (!this.suppressUpdates && this.observerHandler != null) {

//			Set<String> keys = this.txResult.getKeys();
//			for (String key : keys) {
//				ModificationResult modificationResult = this.txResult.getModificationResult(key);
//
//				this.observerHandler.add(key, modificationResult.<StrolchElement> getCreated());
//				this.observerHandler.update(key, modificationResult.<StrolchElement> getUpdated());
//				this.observerHandler.remove(key, modificationResult.<StrolchElement> getDeleted());
//			}
		}
	}

	@Override
	public void autoCloseableRollback() {
//		this.tx.autoCloseableRollback();
	}

	@Override
	public void close() throws StrolchPersistenceException {
		this.closeStrategy.close(this);
	}

	@Override
	public boolean isOpen() {
//		return this.tx.isOpen();
		return true;
	}
}
