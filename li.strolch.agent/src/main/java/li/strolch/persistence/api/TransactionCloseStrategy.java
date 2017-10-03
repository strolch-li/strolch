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
package li.strolch.persistence.api;

import li.strolch.exception.StrolchException;

/**
 * Defines what happens when a {@link StrolchTransaction} is closed. Strolch transactions are auto closeable which means
 * when used in a try-with-resource block, then they are automatically closed. Depending on the use case, these
 * transactions might write something to the DB, or only read the model, or even have to roll back, this is controlled
 * by using these enums.
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public enum TransactionCloseStrategy {

	/**
	 * <p>
	 * This is for read only transactions. If this strategy is used, then no changes will be written to the model. Use
	 * {@link StrolchTransaction#doNothingOnClose()} to simply close the transaction, releasing any resources
	 * </p>
	 * 
	 * <p>
	 * <b>Note:</b> When using this strategy, then the transaction will throw exceptions if you try to add commands to
	 * the transaction.
	 * </p>
	 */
	READ_ONLY() {

		@Override
		public boolean isReadonly() {
			return true;
		}

		@Override
		public void close(StrolchTransaction tx) throws StrolchException {
			tx.autoCloseableReadOnly();
		}
	},

	/**
	 * The main close strategy type where changes are written to the model. Use
	 * {@link StrolchTransaction#commitOnClose()} to commit any changes i.e. commands added to the transaction
	 */
	COMMIT() {

		@Override
		public boolean isReadonly() {
			return false;
		}

		@Override
		public void close(StrolchTransaction tx) throws StrolchException {
			tx.autoCloseableCommit();
		}
	},

	/**
	 * In exceptional cases one might not want any changes to be written to the model, thus calling
	 * {@link StrolchTransaction#rollbackOnClose()} will have the transaction roll back all changes
	 */
	ROLLBACK() {

		@Override
		public boolean isReadonly() {
			return true;
		}

		@Override
		public void close(StrolchTransaction tx) throws StrolchException {
			tx.autoCloseableRollback();
		}
	};

	public abstract void close(StrolchTransaction tx) throws StrolchException;

	public abstract boolean isReadonly();
}