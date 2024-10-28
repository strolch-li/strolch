package li.strolch.agent.api;

import com.google.gson.JsonObject;
import li.strolch.persistence.api.TransactionResult;

import java.time.Duration;

public class AgentStatistics {

	private final Statistics transactions;
	private final Statistics searches;
	private final Statistics services;

	public AgentStatistics() {
		this.transactions = new Statistics();
		this.searches = new Statistics();
		this.services = new Statistics();
	}

	public JsonObject toJson() {
		JsonObject json = new JsonObject();
		json.add("transactions", this.transactions.toJson());
		json.add("searches", this.searches.toJson());
		json.add("services", this.services.toJson());
		return json;
	}

	public void recordTransaction(TransactionResult result) {
		this.transactions.recordEvent(Duration.ofNanos(result.getTxDuration()));
	}

	public void recordSearch(long durationNanos) {
		this.searches.recordEvent(Duration.ofNanos(durationNanos));
	}

	public void recordService(long durationNanos) {
		this.services.recordEvent(Duration.ofNanos(durationNanos));
	}
}
