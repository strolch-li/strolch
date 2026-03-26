/*
 * Copyright (c) 2013-2025 Robert von Burg <eitch@eitchnet.ch>
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

package li.strolch.agent.api;

import com.google.gson.JsonObject;
import li.strolch.persistence.api.TransactionResult;
import li.strolch.persistence.api.TransactionState;

import java.time.Duration;

public class AgentStatistics {

	private final DurationStatistics transactions;
	private final DurationStatistics failedTransactions;
	private final DurationStatistics retriedLocks;
	private final DurationStatistics searches;
	private final DurationStatistics services;
	private final CountStatistics logonsWithActiveUsers;

	public AgentStatistics() {
		this.transactions = new DurationStatistics();
		this.failedTransactions = new DurationStatistics();
		this.retriedLocks = new DurationStatistics();
		this.searches = new DurationStatistics();
		this.services = new DurationStatistics();
		this.logonsWithActiveUsers = new CountStatistics();
	}

	public JsonObject toJson() {
		JsonObject json = new JsonObject();
		json.add("transactions", this.transactions.toJson());
		json.add("failedTransactions", this.failedTransactions.toJson());
		json.add("retriedLocks", this.retriedLocks.toJson());
		json.add("searches", this.searches.toJson());
		json.add("services", this.services.toJson());
		json.add("logonsWithActiveUsers", this.logonsWithActiveUsers.toJson());
		return json;
	}

	public void recordTransaction(TransactionResult result) {
		Duration duration = Duration.ofNanos(result.getTxDuration());
		this.transactions.recordEvent(duration);
		if (result.getState() != TransactionState.CLOSED && result.getState() != TransactionState.COMMITTED)
			this.failedTransactions.recordEvent(duration);
	}

	public void recordRetriedLock() {
		this.retriedLocks.recordEvent(Duration.ZERO);
	}

	public void recordLogon(int activeUsers) {
		this.logonsWithActiveUsers.recordEvent(activeUsers);
	}

	public void recordSearch(long durationNanos) {
		this.searches.recordEvent(Duration.ofNanos(durationNanos));
	}

	public void recordService(long durationNanos) {
		this.services.recordEvent(Duration.ofNanos(durationNanos));
	}
}
