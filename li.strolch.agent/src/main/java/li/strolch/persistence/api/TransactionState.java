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

/**
 * The different states of a {@link StrolchTransaction}
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public enum TransactionState {
	OPEN, // 
	CLOSING, //
	COMMITTING, //
	ROLLING_BACK, //
	CLOSED, //
	COMMITTED, // 
	ROLLED_BACK,
	FAILED;

	/**
	 * @return true if this is {@link #ROLLING_BACK}
	 */
	public boolean isRollingBack() {
		return this == ROLLING_BACK;
	}

	/**
	 * @return true if this is {@link #COMMITTING}
	 */
	public boolean isCommitting() {
		return this == COMMITTING;
	}

	/**
	 * @return true if this is {@link #CLOSING}
	 */
	public boolean isClosing() {
		return this == CLOSING;
	}

	/**
	 * @return true if this is {@link #OPEN}
	 */
	public boolean isOpen() {
		return this == OPEN;
	}

	/**
	 * @return true if this is {@link #CLOSED}
	 */
	public boolean isClosed() {
		return this == CLOSED;
	}

	/**
	 * @return true if this is {@link #FAILED}
	 */
	public boolean isFailed() {
		return this == FAILED;
	}
}
