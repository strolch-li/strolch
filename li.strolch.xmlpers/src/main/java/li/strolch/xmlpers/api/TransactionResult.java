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

import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import li.strolch.utils.helper.StringHelper;

public class TransactionResult {

	private String realm;
	private TransactionState state;
	private Exception failCause;

	private Date startTime;
	private long txDuration;
	private long closeDuration;

	private Map<String, ModificationResult> modificationByKey;

	public TransactionResult() {
		this.state = TransactionState.OPEN;
		this.modificationByKey = new HashMap<>();
	}

	/**
	 * @return the realm
	 */
	public String getRealm() {
		return this.realm;
	}

	/**
	 * @param realm
	 *            the realm to set
	 */
	public void setRealm(String realm) {
		this.realm = realm;
	}

	/**
	 * @return the state
	 */
	public TransactionState getState() {
		return this.state;
	}

	/**
	 * @param state
	 *            the state to set
	 */
	public void setState(TransactionState state) {
		this.state = state;
	}

	/**
	 * The internal exception why the transaction failed
	 * 
	 * @return the failCause
	 */
	public Exception getFailCause() {
		return this.failCause;
	}

	/**
	 * @param failCause
	 *            the failCause to set
	 */
	public void setFailCause(Exception failCause) {
		this.failCause = failCause;
	}

	/**
	 * Start time of the transaction
	 * 
	 * @return the startTime
	 */
	public Date getStartTime() {
		return this.startTime;
	}

	/**
	 * @param startTime
	 *            the startTime to set
	 */
	public void setStartTime(Date startTime) {
		this.startTime = startTime;
	}

	/**
	 * The duration the transaction was open in nanoseconds
	 * 
	 * @return the txDuration
	 */
	public long getTxDuration() {
		return this.txDuration;
	}

	/**
	 * @param txDuration
	 *            the txDuration to set
	 */
	public void setTxDuration(long txDuration) {
		this.txDuration = txDuration;
	}

	/**
	 * The duration the transaction took to close in nanoseconds
	 * 
	 * @return the closeDuration
	 */
	public long getCloseDuration() {
		return this.closeDuration;
	}

	/**
	 * @param closeDuration
	 *            the closeDuration to set
	 */
	public void setCloseDuration(long closeDuration) {
		this.closeDuration = closeDuration;
	}

	/**
	 * @param modificationByKey
	 *            the modificationByKey to set
	 */
	public void setModificationByKey(Map<String, ModificationResult> modificationByKey) {
		this.modificationByKey = modificationByKey;
	}

	/**
	 * @return
	 */
	public Set<String> getKeys() {
		return this.modificationByKey.keySet();
	}

	/**
	 * @param key
	 * @return
	 */
	public ModificationResult getModificationResult(String key) {
		return this.modificationByKey.get(key);
	}

	@SuppressWarnings("nls")
	public String getLogMessage() {

		int nrOfObjects = 0;
		for (ModificationResult result : this.modificationByKey.values()) {
			nrOfObjects += result.getCreated().size();
			nrOfObjects += result.getUpdated().size();
			nrOfObjects += result.getDeleted().size();
		}

		StringBuilder sb = new StringBuilder();
		sb.append("TX for realm ");
		sb.append(getRealm());
		switch (this.state) {
		case OPEN:
			sb.append(" is still open after ");
			break;
		case COMMITTED:
			sb.append(" was completed after ");
			break;
		case ROLLED_BACK:
			sb.append(" was rolled back after ");
			break;
		case FAILED:
			sb.append(" has failed after ");
			break;
		default:
			sb.append(" is in unhandled state ");
			sb.append(this.state);
			sb.append(" after ");
		}

		sb.append(StringHelper.formatNanoDuration(this.txDuration));
		sb.append(" with close operation taking ");
		sb.append(StringHelper.formatNanoDuration(this.closeDuration));
		sb.append(". ");
		sb.append(nrOfObjects);
		sb.append(" objects in ");
		sb.append(this.modificationByKey.size());
		sb.append(" types were modified.");

		return sb.toString();
	}

	/**
	 * Clears all fields of this result, allowing it to be reused
	 */
	public void clear() {
		this.realm = null;
		this.state = null;
		this.failCause = null;
		this.startTime = null;
		this.txDuration = 0L;
		this.closeDuration = 0L;
	}
}
