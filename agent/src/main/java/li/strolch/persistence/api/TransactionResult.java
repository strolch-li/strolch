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

import java.util.Date;

import li.strolch.utils.helper.StringHelper;

public class TransactionResult {

	private String realm;
	private TransactionState state;
	private Exception failCause;

	private final long startNanos;
	private Date startTime;
	private long txDuration;
	private long closeDuration;

	private long created;
	private long read;
	private long updated;
	private long deleted;

	public TransactionResult(String realm, long startNanos, Date startDate) {
		this.realm = realm;
		this.startNanos = startNanos;
		this.startTime = startDate;
		this.state = TransactionState.OPEN;
	}

	/**
	 * @return the realm
	 */
	public String getRealm() {
		return this.realm;
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
	 * @return the startNanos
	 */
	public long getStartNanos() {
		return this.startNanos;
	}

	/**
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
	 * @return the created
	 */
	public long getCreated() {
		return this.created;
	}

	/**
	 * @param created
	 *            the created to set
	 */
	public void setCreated(long created) {
		this.created = created;
	}

	/**
	 * @return the read
	 */
	public long getRead() {
		return this.read;
	}

	/**
	 * @param read
	 *            the read to set
	 */
	public void setRead(long read) {
		this.read = read;
	}

	/**
	 * @return the updated
	 */
	public long getUpdated() {
		return this.updated;
	}

	/**
	 * @param updated
	 *            the updated to set
	 */
	public void setUpdated(long updated) {
		this.updated = updated;
	}

	/**
	 * @return the deleted
	 */
	public long getDeleted() {
		return this.deleted;
	}

	/**
	 * @param deleted
	 *            the deleted to set
	 */
	public void setDeleted(long deleted) {
		this.deleted = deleted;
	}

	public void incCreated(long created) {
		this.created += created;
	}

	public void incRead(long read) {
		this.read += read;
	}

	public void incUpdated(long updated) {
		this.updated += updated;
	}

	public void incDeleted(long deleted) {
		this.deleted += deleted;
	}

	private long getAccessed() {
		return this.created + this.read + this.updated + this.deleted;
	}

	@SuppressWarnings("nls")
	public String getLogMessage() {

		StringBuilder sb = new StringBuilder();
		switch (this.state) {
		case OPEN -> sb.append("TX is still open after ");
		case COMMITTED -> sb.append("TX was completed after ");
		case ROLLED_BACK -> sb.append("TX was rolled back after ");
		case FAILED -> sb.append("TX has failed after ");
		default -> {
			sb.append("TX is in unhandled state ");
			sb.append(this.state);
			sb.append(" after ");
		}
		}

		sb.append(StringHelper.formatNanoDuration(this.txDuration));
		sb.append(" with close operation taking ");
		sb.append(StringHelper.formatNanoDuration(this.closeDuration));
		sb.append(". ");
		sb.append(getAccessed());
		sb.append(" objects accessed (");
		sb.append(this.created);
		sb.append(" created, ");
		sb.append(this.read);
		sb.append(" read, ");
		sb.append(this.updated);
		sb.append(" updated, ");
		sb.append(this.deleted);
		sb.append(" deleted, ");

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
		this.created = 0L;
		this.read = 0L;
		this.updated = 0L;
		this.deleted = 0L;
	}
}
