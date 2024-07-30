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

	public TransactionState getState() {
		return this.state;
	}

	public void setState(TransactionState state) {
		this.state = state;
	}

	public Exception getFailCause() {
		return this.failCause;
	}

	public void setFailCause(Exception failCause) {
		this.failCause = failCause;
	}

	public Date getStartTime() {
		return this.startTime;
	}

	public void setStartTime(Date startTime) {
		this.startTime = startTime;
	}

	public long getTxDuration() {
		return this.txDuration;
	}

	public void setTxDuration(long txDuration) {
		this.txDuration = txDuration;
	}

	public long getCloseDuration() {
		return this.closeDuration;
	}

	public void setCloseDuration(long closeDuration) {
		this.closeDuration = closeDuration;
	}

	public void setModificationByKey(Map<String, ModificationResult> modificationByKey) {
		this.modificationByKey = modificationByKey;
	}

	public Set<String> getKeys() {
		return this.modificationByKey.keySet();
	}

	public ModificationResult getModificationResult(String key) {
		return this.modificationByKey.get(key);
	}

	public String getLogMessage() {

		int nrOfObjects = 0;
		for (ModificationResult result : this.modificationByKey.values()) {
			nrOfObjects += result.getCreated().size();
			nrOfObjects += result.getUpdated().size();
			nrOfObjects += result.getDeleted().size();
		}

		StringBuilder sb = new StringBuilder();
		sb.append("TX");
		switch (this.state) {
		case OPEN -> sb.append(" is still open after ");
		case COMMITTED -> sb.append(" was completed after ");
		case ROLLED_BACK -> sb.append(" was rolled back after ");
		case FAILED -> sb.append(" has failed after ");
		default -> {
			sb.append(" is in unhandled state ");
			sb.append(this.state);
			sb.append(" after ");
		}
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
		this.state = null;
		this.failCause = null;
		this.startTime = null;
		this.txDuration = 0L;
		this.closeDuration = 0L;
	}
}
