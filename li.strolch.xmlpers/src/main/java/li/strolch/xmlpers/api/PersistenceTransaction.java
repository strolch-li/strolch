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
package li.strolch.xmlpers.api;

import li.strolch.xmlpers.objref.LockableObject;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public interface PersistenceTransaction extends AutoCloseable {

	/**
	 * Return true if the TX already has a {@link TransactionResult}
	 *
	 * @return true if the TX already has a {@link TransactionResult}
	 */
	public boolean hasTransactionResult();

	/**
	 * Returns the {@link TransactionResult} for this transaction
	 *
	 * @return the {@link TransactionResult}
	 *
	 * @throws IllegalStateException
	 * 		if the transaction has not yet been closed
	 */
	public TransactionResult getTransactionResult() throws IllegalStateException;

	/**
	 * @throws IllegalStateException
	 * 		if a result is already set
	 */
	public void setTransactionResult(TransactionResult txResult) throws IllegalStateException;

	public void setCloseStrategy(TransactionCloseStrategy closeStrategy);

	public void autoCloseableCommit();

	public void autoCloseableRollback();

	@Override
	public void close() throws XmlPersistenceException;

	public boolean isOpen();

	public ObjectDao getObjectDao();

	public MetadataDao getMetadataDao();

	public FileDao getFileDao();

	public PersistenceManager getManager();

	void lock(LockableObject lockableObject);
}