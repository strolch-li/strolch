/*
 * Copyright (c) 2026 Robert von Burg <eitch@eitchnet.ch>
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

package li.strolch.job;

import com.google.gson.*;
import li.strolch.agent.api.StrolchRealm;
import li.strolch.job.ResourceMonitorJob.Configuration;
import li.strolch.job.ResourceMonitorJob.NotificationMode;
import li.strolch.job.ResourceMonitorSampler.Metric;
import li.strolch.job.ResourceMonitorSampler.Snapshot;
import li.strolch.job.ResourceMonitorSampler.Status;
import li.strolch.model.Resource;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Certificate;

import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.time.Instant;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

final class ResourceMonitorStateStore {

	static final String STATE_RESOURCE_TYPE = "StrolchResourceMonitorState";
	private static final String STATE_PARAMETER = "stateJson";
	private static final int STATE_SCHEMA_VERSION = 1;
	private static final Map<String, Object> EXECUTION_LOCKS = new ConcurrentHashMap<>();

	static Object lockFor(String realmName, String jobId, String monitorId) {
		String key = realmName + "\u0000" + jobId + "\u0000" + monitorId;
		return EXECUTION_LOCKS.computeIfAbsent(key, _ -> new Object());
	}

	Update recordSample(StrolchRealm realm, Certificate certificate, String jobId, Configuration configuration,
			Snapshot snapshot) {
		String realmName = realm.getRealm();
		synchronized (lockFor(realmName, jobId, configuration.monitorId())) {
			try (StrolchTransaction tx = realm
					.openTx(certificate, ResourceMonitorStateStore.class, false)
					.rollbackOnFailure()) {
				Resource resource = readOrCreateStateResource(tx, jobId, configuration.monitorId());
				MutableState state = decode(resource.getString(STATE_PARAMETER));
				Evaluation evaluation = updateMetricState(state, configuration, snapshot);
				List<Event> events = evaluation.events();
				String createdNotificationId = null;
				if (configuration.notificationMode() == NotificationMode.ALWAYS || !events.isEmpty()) {
					createdNotificationId = UUID.randomUUID().toString();
					state.pending.add(
							new PendingNotification(createdNotificationId, snapshot, configuration.notificationMode(),
									List.copyOf(events)));
				}

				resource.setString(STATE_PARAMETER, encode(state));
				persist(tx, resource);
				tx.commitOnClose();
				return new Update(List.copyOf(events), List.copyOf(state.pending), evaluation.decisions(),
						createdNotificationId);
			}
		}
	}

	void acknowledge(StrolchRealm realm, Certificate certificate, String jobId, String monitorId, String notificationId,
			Instant acknowledgedAt) {
		String realmName = realm.getRealm();
		synchronized (lockFor(realmName, jobId, monitorId)) {
			try (StrolchTransaction tx = realm
					.openTx(certificate, ResourceMonitorStateStore.class, false)
					.rollbackOnFailure()) {
				Resource resource = tx.getResourceBy(STATE_RESOURCE_TYPE, stateResourceId(realmName, jobId, monitorId),
						true);
				resource = tx.readLock(resource);
				MutableState state = decode(resource.getString(STATE_PARAMETER));
				if (state.pending.isEmpty() || !state.pending.getFirst().id().equals(notificationId))
					throw new IllegalStateException("Notifications must be acknowledged in pending order");

				PendingNotification acknowledged = state.pending.removeFirst();
				for (Event event : acknowledged.events()) {
					MetricState metricState = state.metrics.get(event.metricKey());
					if (metricState == null)
						continue;

					if ((event.type() == EventType.BREACH || event.type() == EventType.REMINDER)
							&& metricState.breached()
							&& Objects.equals(metricState.breachStartedAt(), event.breachStartedAt())) {
						state.metrics.put(event.metricKey(),
								new MetricState(true, metricState.breachStartedAt(), acknowledgedAt));
					} else if (event.type() == EventType.RECOVERY && !metricState.breached()) {
						state.metrics.put(event.metricKey(), new MetricState(false, null, null));
					}
				}

				resource.setString(STATE_PARAMETER, encode(state));
				tx.update(resource);
				tx.commitOnClose();
			}
		}
	}

	private static Evaluation updateMetricState(MutableState state, Configuration configuration, Snapshot snapshot) {
		List<Event> events = new ArrayList<>();
		List<Decision> decisions = new ArrayList<>();
		for (Metric metric : snapshot.metrics()) {
			MetricState previous = state.metrics.get(metric.key());
			if (metric.status() == Status.UNAVAILABLE) {
				decisions.add(decision(metric, DecisionReason.MEASUREMENT_UNAVAILABLE));
				continue;
			}

			if (metric.status() == Status.BREACHED) {
				if (previous == null || !previous.breached()) {
					Instant breachStartedAt = snapshot.sampledAt();
					state.metrics.put(metric.key(), new MetricState(true, breachStartedAt, null));
					events.add(new Event(metric.key(), EventType.BREACH, breachStartedAt));
					decisions.add(decision(metric, DecisionReason.BREACH_EVENT_QUEUED));
					continue;
				}

				state.metrics.put(metric.key(), previous);
				if (configuration.notificationMode() == NotificationMode.ALWAYS) {
					decisions.add(decision(metric, DecisionReason.ALWAYS_MODE_REPORT));
				} else if (previous.lastSuccessfulNotificationAt() == null) {
					boolean pending = hasPendingBreachNotification(state, metric.key(), previous.breachStartedAt());
					decisions.add(decision(metric, pending ? DecisionReason.INITIAL_BREACH_NOTIFICATION_PENDING :
							DecisionReason.INITIAL_BREACH_NOTIFICATION_NOT_ACKNOWLEDGED));
				} else if (Duration
						.between(previous.lastSuccessfulNotificationAt(), snapshot.sampledAt())
						.compareTo(configuration.reminderInterval()) < 0) {
					decisions.add(decision(metric, DecisionReason.REMINDER_INTERVAL_NOT_ELAPSED));
				} else if (hasPendingBreachNotification(state, metric.key(), previous.breachStartedAt())) {
					decisions.add(decision(metric, DecisionReason.REMINDER_ALREADY_PENDING));
				} else {
					events.add(new Event(metric.key(), EventType.REMINDER, previous.breachStartedAt()));
					decisions.add(decision(metric, DecisionReason.REMINDER_EVENT_QUEUED));
				}
			} else {
				if (previous != null && previous.breached()) {
					events.add(new Event(metric.key(), EventType.RECOVERY, previous.breachStartedAt()));
					decisions.add(decision(metric, DecisionReason.RECOVERY_EVENT_QUEUED));
				} else {
					decisions.add(decision(metric, configuration.notificationMode() == NotificationMode.ALWAYS ?
							DecisionReason.ALWAYS_MODE_REPORT : DecisionReason.BELOW_THRESHOLD_NO_ALERT));
				}
				state.metrics.put(metric.key(), new MetricState(false, null, null));
			}
		}
		return new Evaluation(List.copyOf(events), List.copyOf(decisions));
	}

	private static Decision decision(Metric metric, DecisionReason reason) {
		return new Decision(metric.key(), metric.status(), metric.value(), metric.threshold(), metric.totalBytes(),
				metric.freeBytes(), metric.usableBytes(), metric.usedBytes(), metric.sourceRatio(), reason);
	}

	private static boolean hasPendingBreachNotification(MutableState state, String metricKey, Instant breachStartedAt) {
		for (PendingNotification pending : state.pending) {
			for (Event event : pending.events()) {
				if ((event.type() == EventType.BREACH || event.type() == EventType.REMINDER) && event
						.metricKey()
						.equals(metricKey) && Objects.equals(event.breachStartedAt(), breachStartedAt))
					return true;
			}
		}
		return false;
	}

	private static Resource readOrCreateStateResource(StrolchTransaction tx, String jobId, String monitorId) {
		String realmName = tx.getRealmName();
		String resourceId = stateResourceId(realmName, jobId, monitorId);
		Resource resource = tx.getResourceBy(STATE_RESOURCE_TYPE, resourceId);
		if (resource == null) {
			resource = new Resource(resourceId, "Resource Monitor State " + monitorId, STATE_RESOURCE_TYPE);
			resource.setString("jobId", jobId);
			resource.setString("monitorId", monitorId);
			resource.setString("realm", realmName);
		} else {
			resource = tx.readLock(resource);
		}
		return resource;
	}

	static String stateResourceId(String realmName, String jobId, String monitorId) {
		String key = realmName + "\u0000" + jobId + "\u0000" + monitorId;
		return "resource-monitor-" + UUID.nameUUIDFromBytes(key.getBytes(StandardCharsets.UTF_8));
	}

	private static void persist(StrolchTransaction tx, Resource resource) {
		if (tx.getResourceBy(STATE_RESOURCE_TYPE, resource.getId()) == null)
			tx.add(resource);
		else
			tx.update(resource);
	}

	private static MutableState decode(String json) {
		if (json == null || json.isBlank())
			return new MutableState();

		JsonObject root = JsonParser.parseString(json).getAsJsonObject();
		if (root.get("schemaVersion").getAsInt() != STATE_SCHEMA_VERSION)
			throw new IllegalStateException("Unsupported resource monitor state schema version");

		MutableState state = new MutableState();
		JsonObject metrics = root.getAsJsonObject("metrics");
		for (Map.Entry<String, JsonElement> entry : metrics.entrySet()) {
			JsonObject metric = entry.getValue().getAsJsonObject();
			state.metrics.put(entry.getKey(),
					new MetricState(metric.get("breached").getAsBoolean(), readInstant(metric, "breachStartedAt"),
							readInstant(metric, "lastSuccessfulNotificationAt")));
		}
		JsonArray pending = root.getAsJsonArray("pending");
		for (JsonElement element : pending)
			state.pending.add(decodePending(element.getAsJsonObject()));
		return state;
	}

	private static String encode(MutableState state) {
		JsonObject root = new JsonObject();
		root.addProperty("schemaVersion", STATE_SCHEMA_VERSION);
		JsonObject metrics = new JsonObject();
		for (Map.Entry<String, MetricState> entry : state.metrics.entrySet()) {
			JsonObject metric = new JsonObject();
			metric.addProperty("breached", entry.getValue().breached());
			addInstant(metric, "breachStartedAt", entry.getValue().breachStartedAt());
			addInstant(metric, "lastSuccessfulNotificationAt", entry.getValue().lastSuccessfulNotificationAt());
			metrics.add(entry.getKey(), metric);
		}
		root.add("metrics", metrics);

		JsonArray pending = new JsonArray();
		for (PendingNotification notification : state.pending)
			pending.add(encodePending(notification));
		root.add("pending", pending);
		return root.toString();
	}

	private static JsonObject encodePending(PendingNotification notification) {
		JsonObject json = new JsonObject();
		json.addProperty("id", notification.id());
		json.addProperty("notificationMode", notification.notificationMode().name());
		json.add("snapshot", encodeSnapshot(notification.snapshot()));
		JsonArray events = new JsonArray();
		for (Event event : notification.events()) {
			JsonObject eventJson = new JsonObject();
			eventJson.addProperty("metricKey", event.metricKey());
			eventJson.addProperty("type", event.type().name());
			addInstant(eventJson, "breachStartedAt", event.breachStartedAt());
			events.add(eventJson);
		}
		json.add("events", events);
		return json;
	}

	static JsonObject generateReport(String realmName, String jobId, String monitorId,
			PendingNotification notification) {
		JsonObject pending = encodePending(notification);
		JsonObject snapshot = pending.getAsJsonObject("snapshot");
		JsonObject report = new JsonObject();
		report.addProperty("schemaVersion", 1);
		report.addProperty("monitorId", monitorId);
		report.addProperty("jobId", jobId);
		report.addProperty("realm", realmName);
		report.add("sampledAt", snapshot.get("sampledAt"));
		report.add("notificationMode", pending.get("notificationMode"));
		report.add("events", pending.get("events"));
		report.add("metrics", snapshot.get("metrics"));
		return report;
	}

	private static PendingNotification decodePending(JsonObject json) {
		List<Event> events = new ArrayList<>();
		for (JsonElement element : json.getAsJsonArray("events")) {
			JsonObject event = element.getAsJsonObject();
			events.add(
					new Event(event.get("metricKey").getAsString(), EventType.valueOf(event.get("type").getAsString()),
							readInstant(event, "breachStartedAt")));
		}
		return new PendingNotification(json.get("id").getAsString(), decodeSnapshot(json.getAsJsonObject("snapshot")),
				NotificationMode.valueOf(json.get("notificationMode").getAsString()), List.copyOf(events));
	}

	private static JsonObject encodeSnapshot(Snapshot snapshot) {
		JsonObject json = new JsonObject();
		json.addProperty("sampledAt", snapshot.sampledAt().toString());
		JsonArray metrics = new JsonArray();
		for (Metric metric : snapshot.metrics()) {
			JsonObject metricJson = new JsonObject();
			metricJson.addProperty("key", metric.key());
			metricJson.addProperty("unit", metric.unit());
			addNumber(metricJson, "value", metric.value());
			metricJson.addProperty("threshold", metric.threshold());
			metricJson.addProperty("status", metric.status().name());
			addNumber(metricJson, "totalBytes", metric.totalBytes());
			addNumber(metricJson, "freeBytes", metric.freeBytes());
			addNumber(metricJson, "usableBytes", metric.usableBytes());
			addNumber(metricJson, "usedBytes", metric.usedBytes());
			metrics.add(metricJson);
		}
		json.add("metrics", metrics);
		return json;
	}

	private static Snapshot decodeSnapshot(JsonObject json) {
		List<Metric> metrics = new ArrayList<>();
		for (JsonElement element : json.getAsJsonArray("metrics")) {
			JsonObject metric = element.getAsJsonObject();
			metrics.add(
					new Metric(metric.get("key").getAsString(), metric.get("unit").getAsString(), readDouble(metric),
							metric.get("threshold").getAsDouble(), Status.valueOf(metric.get("status").getAsString()),
							readLong(metric, "totalBytes"), readLong(metric, "freeBytes"),
							readLong(metric, "usableBytes"), readLong(metric, "usedBytes")));
		}
		return new Snapshot(Instant.parse(json.get("sampledAt").getAsString()), List.copyOf(metrics));
	}

	private static void addInstant(JsonObject json, String name, Instant value) {
		if (value == null)
			json.add(name, JsonNull.INSTANCE);
		else
			json.addProperty(name, value.toString());
	}

	private static Instant readInstant(JsonObject json, String name) {
		JsonElement value = json.get(name);
		return value == null || value.isJsonNull() ? null : Instant.parse(value.getAsString());
	}

	private static void addNumber(JsonObject json, String name, Number value) {
		if (value == null)
			json.add(name, JsonNull.INSTANCE);
		else
			json.addProperty(name, value);
	}

	private static Double readDouble(JsonObject json) {
		JsonElement value = json.get("value");
		return value == null || value.isJsonNull() ? null : value.getAsDouble();
	}

	private static Long readLong(JsonObject json, String name) {
		JsonElement value = json.get(name);
		return value == null || value.isJsonNull() ? null : value.getAsLong();
	}

	enum EventType {
		BREACH,
		REMINDER,
		RECOVERY
	}

	record Event(String metricKey, EventType type, Instant breachStartedAt) {
	}

	record PendingNotification(String id, Snapshot snapshot, NotificationMode notificationMode, List<Event> events) {
	}

	record Update(List<Event> events, List<PendingNotification> pending, List<Decision> decisions,
				  String createdNotificationId) {
	}

	record Decision(String metricKey, Status status, Double value, double threshold, Long totalBytes, Long freeBytes,
					Long usableBytes, Long usedBytes, Double sourceRatio, DecisionReason reason) {
	}

	private record Evaluation(List<Event> events, List<Decision> decisions) {
	}

	enum DecisionReason {
		MEASUREMENT_UNAVAILABLE("measurement unavailable; no state transition or alert was inferred"),
		BREACH_EVENT_QUEUED("value reached threshold; breach event queued"),
		REMINDER_EVENT_QUEUED("breach continues and reminder interval elapsed; reminder event queued"),
		INITIAL_BREACH_NOTIFICATION_PENDING("initial breach notification is pending delivery"),
		INITIAL_BREACH_NOTIFICATION_NOT_ACKNOWLEDGED("initial breach notification has not been acknowledged"),
		REMINDER_INTERVAL_NOT_ELAPSED("breach continues but reminder interval has not elapsed"),
		REMINDER_ALREADY_PENDING("breach continues but a reminder notification is already pending"),
		ALWAYS_MODE_REPORT("no threshold transition; ALWAYS mode queues a report for every sample"),
		RECOVERY_EVENT_QUEUED("value fell below threshold after a breach; recovery event queued"),
		BELOW_THRESHOLD_NO_ALERT("value is below threshold and there is no active breach");

		private final String description;

		DecisionReason(String description) {
			this.description = description;
		}

		String description() {
			return this.description;
		}
	}

	private record MetricState(boolean breached, Instant breachStartedAt, Instant lastSuccessfulNotificationAt) {
	}

	private static class MutableState {

		private final Map<String, MetricState> metrics = new LinkedHashMap<>();
		private final List<PendingNotification> pending = new ArrayList<>();
	}
}
