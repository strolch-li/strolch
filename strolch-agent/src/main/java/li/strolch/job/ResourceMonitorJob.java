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

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.google.gson.Strictness;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonToken;
import jakarta.mail.internet.AddressException;
import jakarta.mail.internet.InternetAddress;
import li.strolch.agent.api.*;
import li.strolch.handler.mail.MailHandler;
import li.strolch.handler.mail.SimulatedMailHandler;
import li.strolch.handler.operationslog.OperationsLog;
import li.strolch.job.ResourceMonitorSampler.Snapshot;
import li.strolch.job.ResourceMonitorStateStore.Decision;
import li.strolch.job.ResourceMonitorStateStore.EventType;
import li.strolch.job.ResourceMonitorStateStore.PendingNotification;
import li.strolch.job.ResourceMonitorStateStore.Update;
import li.strolch.model.Locator;
import li.strolch.model.Resource;
import li.strolch.model.log.LogMessage;
import li.strolch.model.log.LogMessageState;
import li.strolch.model.log.LogSeverity;
import li.strolch.model.parameter.Parameter;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Certificate;
import li.strolch.privilege.model.PrivilegeContext;
import li.strolch.utils.MailAttachment;
import li.strolch.utils.helper.SystemHelper;
import org.jetbrains.annotations.NotNull;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.io.StringReader;
import java.time.Duration;
import java.time.Instant;
import java.time.format.DateTimeParseException;
import java.util.*;

import static java.text.MessageFormat.format;
import static li.strolch.agent.api.StrolchAgent.getUniqueId;
import static li.strolch.job.ResourceMonitorStateStore.generateReport;
import static li.strolch.job.ResourceMonitorStateStore.lockFor;
import static li.strolch.model.Tags.AGENT;
import static li.strolch.runtime.StrolchConstants.SYSTEM_USER_AGENT;
import static li.strolch.runtime.StrolchConstants.TYPE_STROLCH_JOB;

public class ResourceMonitorJob extends StrolchJob {

	private static final Logger logger = LoggerFactory.getLogger(ResourceMonitorJob.class);

	private final ResourceMonitorSampler sampler;

	public ResourceMonitorJob(StrolchAgent agent, String id, String name, JobMode jobMode) {
		this(agent, id, name, jobMode, new ResourceMonitorSampler());
	}

	ResourceMonitorJob(StrolchAgent agent, String id, String name, JobMode jobMode, ResourceMonitorSampler sampler) {
		super(agent, id, name, jobMode);
		this.sampler = sampler;
	}

	@Override
	protected void execute(PrivilegeContext ctx) throws Exception {
		Configuration configuration = readConfiguration(ctx.certificate());
		String realmName = getSourceRealmName();
		StrolchRealm realm = getAgent().getRealm(realmName);
		synchronized (lockFor(realmName, getId(), configuration.monitorId())) {
			Snapshot snapshot = this.sampler.sample(configuration);
			ResourceMonitorStateStore store = new ResourceMonitorStateStore();
			Update update = store.recordSample(realm, ctx.certificate(), getId(), configuration, snapshot);
			if (configuration.verbose()) {
				logger.info("Resource monitor {} in realm {} sampled at {} in {} mode", configuration.monitorId(),
						realmName, snapshot.sampledAt(), configuration.notificationMode());
				for (Decision decision : update.decisions()) {
					String value = decision.value() == null ? "unavailable" : decision.value() + "%";
					logger.info("Resource monitor {} metric {}: status={}, value={}, threshold={}%; inputs={}; {}",
							configuration.monitorId(), decision.metricKey(), decision.status(), value,
							decision.threshold(), sourceValues(decision), decision.reason().description());
				}
				if (update.createdNotificationId() == null)
					logger.info(
							"Resource monitor {} did not queue a new report because no notification condition was met",
							configuration.monitorId());
			}

			for (PendingNotification pending : update.pending()) {
				boolean retry = !pending.id().equals(update.createdNotificationId());
				if (configuration.verbose())
					logger.info("Resource monitor {} {} report {} sampled at {} with events {}",
							configuration.monitorId(), retry ? "retrying" : "sending", pending.id(),
							pending.snapshot().sampledAt(), pending.events());
				try {
					deliverNotification(configuration, pending);
				} catch (Exception e) {
					if (configuration.verbose())
						logger.warn("Resource monitor {} delivery failed for report {}; it remains pending",
								configuration.monitorId(), pending.id(), e);
					throw e;
				}
				try {
					store.acknowledge(realm, ctx.certificate(), getId(), configuration.monitorId(), pending.id(),
							Instant.now());
				} catch (Exception e) {
					if (configuration.verbose())
						logger.warn(
								"Resource monitor {} sent report {} but could not acknowledge it; a retry may duplicate the email",
								configuration.monitorId(), pending.id(), e);
					throw e;
				}
				if (configuration.verbose())
					logger.info("Resource monitor {} delivered and acknowledged report {}", configuration.monitorId(),
							pending.id());
			}
		}
	}

	private static String sourceValues(Decision decision) {
		String key = decision.metricKey();
		if ("systemCpu".equals(key) || "processCpu".equals(key))
			return "sourceRatio=" + (decision.sourceRatio() == null ? "unavailable" : decision.sourceRatio());
		if (key.startsWith("disk:"))
			return format("usedBytes={0}, freeBytes={1}, totalBytes={2}, usableBytes={3}", number(decision.usedBytes()),
					number(decision.freeBytes()), number(decision.totalBytes()), number(decision.usableBytes()));
		if ("systemMemory".equals(key))
			return format("usedBytes={0}, freeBytes={1}, totalBytes={2}", number(decision.usedBytes()),
					number(decision.freeBytes()), number(decision.totalBytes()));
		if ("jvmHeap".equals(key))
			return format("usedBytes={0}, freeBytes={1}, maxBytes={2}", number(decision.usedBytes()),
					number(decision.freeBytes()), number(decision.totalBytes()));
		return "unavailable";
	}

	private static String number(Long value) {
		return value == null ? "unavailable" : value.toString();
	}

	void deliverNotification(Configuration configuration, PendingNotification pending) throws Exception {
		if (Thread.currentThread().isInterrupted())
			throw new InterruptedException("Resource monitor delivery interrupted");

		String recipients = validateRecipients(configuration.recipients());
		MailHandler mail = getComponent(MailHandler.class);
		if (!(mail instanceof SimulatedMailHandler) && (!mail.isEncryptionEnabled() || !mail.isSigningEnabled()))
			throw new IllegalStateException("Resource monitor requires mail encryption and signing");

		getComponent(OperationsLog.class).addMessage(createLogMessage(isWarning(pending)));

		JsonObject report = generateReport(getSourceRealmName(), getId(), configuration.monitorId(), pending);
		report.add("operatingSystem", operatingSystemMetadata());
		report.add("jdk", jdkMetadata());
		addApplicationMetadata(report);

		mail.sendEncryptedMailWithAttachment(recipients, "Resource monitor report",
				"A resource monitor report is attached.",
				new MailAttachment(report.toString(), "resource-monitor_" + getUniqueId() + ".json", "application/json",
						true, true));

		if (Thread.currentThread().isInterrupted())
			throw new InterruptedException("Resource monitor delivery interrupted");
	}

	@NotNull
	private LogMessage createLogMessage(boolean warning) {
		return new LogMessage(getSourceRealmName(), SYSTEM_USER_AGENT,
				Locator.valueOf(AGENT, ResourceMonitorJob.class.getSimpleName(), getId()),
				warning ? LogSeverity.Warning : LogSeverity.Info, LogMessageState.Information,
				ResourceBundle.getBundle("strolch-agent"), "job.resourceMonitor.report");
	}

	private static boolean isWarning(PendingNotification pending) {
		return pending.events().stream().anyMatch(event -> event.type() != EventType.RECOVERY) || pending
				.snapshot()
				.metrics()
				.stream()
				.anyMatch(metric -> metric.status() == ResourceMonitorSampler.Status.BREACHED);
	}

	private static JsonObject operatingSystemMetadata() {
		JsonObject operatingSystem = new JsonObject();
		operatingSystem.addProperty("name", SystemHelper.osName);
		operatingSystem.addProperty("version", SystemHelper.osVersion);
		return operatingSystem;
	}

	private static JsonObject jdkMetadata() {
		JsonObject jdk = new JsonObject();
		jdk.addProperty("vendor", SystemHelper.javaVendor);
		jdk.addProperty("type", System.getProperty("java.vm.name"));
		jdk.addProperty("version", SystemHelper.javaVersion);
		return jdk;
	}

	private void addApplicationMetadata(JsonObject report) {
		VersionQueryResult versions = getAgent().getVersion();
		StrolchVersion applicationVersion = versions == null ? null : versions.getAppVersion();
		AgentVersion agentVersion = versions == null ? null : versions.getAgentVersion();

		JsonObject application = new JsonObject();
		application.addProperty("name", agentVersion == null ? null : agentVersion.getAgentName());
		application.addProperty("environment", agentVersion == null ? null : agentVersion.getEnvironment());
		application.addProperty("version", applicationVersion == null ? null : applicationVersion.getArtifactVersion());
		report.add("application", application);

		JsonObject strolchAgent = new JsonObject();
		strolchAgent.addProperty("groupId", agentVersion == null ? null : agentVersion.getGroupId());
		strolchAgent.addProperty("artifactId", agentVersion == null ? null : agentVersion.getArtifactId());
		strolchAgent.addProperty("version", agentVersion == null ? null : agentVersion.getArtifactVersion());
		report.add("strolchAgent", strolchAgent);
	}

	private static String validateRecipients(List<String> recipients) {
		if (recipients == null || recipients.isEmpty())
			throw new IllegalArgumentException("Resource monitor requires recipients");
		try {
			for (String recipient : recipients) {
				if (recipient == null || recipient.isBlank())
					throw new IllegalArgumentException("Resource monitor recipients must not be blank");
				InternetAddress[] addresses = InternetAddress.parse(recipient, true);
				if (addresses.length != 1 || addresses[0].isGroup())
					throw new IllegalArgumentException("Each resource monitor recipient must be one email address");
				addresses[0].validate();
			}
		} catch (AddressException e) {
			throw new IllegalArgumentException("Invalid resource monitor recipient", e);
		}
		return String.join(",", recipients);
	}

	Configuration readConfiguration(Certificate cert) {
		String realmName = getSourceRealmName();
		if (realmName == null || realmName.isBlank())
			throw new IllegalStateException("Resource monitor job " + getId() + " has no source realm");

		try (StrolchTransaction tx = openTx(realmName, cert, true)) {
			Resource job = tx.getResourceBy(TYPE_STROLCH_JOB, getId(), true);
			return Configuration.from(job);
		}
	}

	enum NotificationMode {
		ALWAYS,
		THRESHOLD
	}

	record Configuration(String monitorId, NotificationMode notificationMode, List<String> recipients,
						 Duration reminderInterval, double diskThresholdPercent,
						 Map<String, Double> diskThresholdOverrides, double systemMemoryThresholdPercent,
						 double systemCpuThresholdPercent, double jvmHeapThresholdPercent,
						 double processCpuThresholdPercent, boolean verbose) {

		Configuration(String monitorId, NotificationMode notificationMode, List<String> recipients,
				Duration reminderInterval, double diskThresholdPercent, Map<String, Double> diskThresholdOverrides,
				double systemMemoryThresholdPercent, double systemCpuThresholdPercent, double jvmHeapThresholdPercent,
				double processCpuThresholdPercent) {
			this(monitorId, notificationMode, recipients, reminderInterval, diskThresholdPercent,
					diskThresholdOverrides, systemMemoryThresholdPercent, systemCpuThresholdPercent,
					jvmHeapThresholdPercent, processCpuThresholdPercent, false);
		}

		static Configuration from(Resource job) {
			String monitorId = requiredString(job, "monitorId");
			NotificationMode notificationMode;
			try {
				notificationMode = NotificationMode.valueOf(
						requiredString(job, "notificationMode").toUpperCase(Locale.ROOT));
			} catch (IllegalArgumentException e) {
				throw new IllegalArgumentException("notificationMode must be ALWAYS or THRESHOLD", e);
			}

			List<String> recipients = new ArrayList<>();
			for (String recipient : job.getStringList("recipients")) {
				if (recipient == null || recipient.isBlank())
					throw new IllegalArgumentException("recipients must not contain blank values");
				recipients.add(recipient.trim());
			}
			if (recipients.isEmpty())
				throw new IllegalArgumentException("recipients must contain at least one recipient");

			Duration reminderInterval;
			try {
				reminderInterval = Duration.parse(requiredString(job, "reminderInterval"));
			} catch (DateTimeParseException e) {
				throw new IllegalArgumentException("reminderInterval must be a positive ISO-8601 duration", e);
			}
			if (reminderInterval.isZero() || reminderInterval.isNegative())
				throw new IllegalArgumentException("reminderInterval must be positive");

			Map<String, Double> overrides = parseDiskOverrides(job);
			return new Configuration(monitorId, notificationMode, List.copyOf(recipients), reminderInterval,
					percentage(job, "diskThresholdPercent"), overrides, percentage(job, "systemMemoryThresholdPercent"),
					percentage(job, "systemCpuThresholdPercent"), percentage(job, "jvmHeapThresholdPercent"),
					percentage(job, "processCpuThresholdPercent"), job.hasParameter("verbose") && job.is("verbose"));
		}

		double diskThresholdFor(String rootPath) {
			return this.diskThresholdOverrides.getOrDefault(rootPath, this.diskThresholdPercent);
		}

		private static String requiredString(Resource job, String parameterName) {
			String value = job.getString(parameterName);
			if (value == null || value.isBlank())
				throw new IllegalArgumentException(parameterName + " must not be blank");
			return value.trim();
		}

		private static double percentage(Resource job, String parameterName) {
			Parameter<?> parameter = job.getParameter(parameterName, true);
			Object value = parameter.getValue();
			double percentage;
			try {
				percentage = value instanceof Number number ? number.doubleValue() :
						Double.parseDouble(Objects.toString(value));
			} catch (NumberFormatException e) {
				throw new IllegalArgumentException(parameterName + " must be a number in (0, 100]", e);
			}
			if (!Double.isFinite(percentage) || percentage <= 0.0 || percentage > 100.0)
				throw new IllegalArgumentException(parameterName + " must be in (0, 100]");
			return percentage;
		}

		private static Map<String, Double> parseDiskOverrides(Resource job) {
			if (!job.hasParameter("diskThresholdOverrides"))
				return Map.of();

			String json = requiredString(job, "diskThresholdOverrides");
			JsonElement root;
			try (JsonReader reader = new JsonReader(new StringReader(json))) {
				reader.setStrictness(Strictness.STRICT);
				root = JsonParser.parseReader(reader);
				if (reader.peek() != JsonToken.END_DOCUMENT)
					throw new IllegalArgumentException("Unexpected trailing content");
			} catch (IOException | RuntimeException e) {
				throw new IllegalArgumentException("diskThresholdOverrides must be a JSON object", e);
			}
			if (!root.isJsonObject())
				throw new IllegalArgumentException("diskThresholdOverrides must be a JSON object");

			JsonObject overridesJson = root.getAsJsonObject();
			Map<String, Double> overrides = new LinkedHashMap<>();
			for (Map.Entry<String, JsonElement> entry : overridesJson.entrySet()) {
				if (entry.getKey().isBlank() || !entry.getValue().isJsonPrimitive() || !entry
						.getValue()
						.getAsJsonPrimitive()
						.isNumber())
					throw new IllegalArgumentException("diskThresholdOverrides must map root paths to percentages");
				double percentage = entry.getValue().getAsDouble();
				if (!Double.isFinite(percentage) || percentage <= 0.0 || percentage > 100.0)
					throw new IllegalArgumentException("Disk override for " + entry.getKey() + " must be in (0, 100]");
				overrides.put(entry.getKey(), percentage);
			}
			return Map.copyOf(overrides);
		}
	}
}
