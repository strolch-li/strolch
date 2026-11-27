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

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import li.strolch.RuntimeMock;
import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.StrolchVersion;
import li.strolch.agent.api.VersionQueryResult;
import li.strolch.handler.mail.MailHandler;
import li.strolch.handler.operationslog.OperationsLog;
import li.strolch.job.ResourceMonitorJob.Configuration;
import li.strolch.job.ResourceMonitorJob.NotificationMode;
import li.strolch.job.ResourceMonitorSampler.Metric;
import li.strolch.job.ResourceMonitorSampler.Snapshot;
import li.strolch.job.ResourceMonitorSampler.Status;
import li.strolch.job.ResourceMonitorStateStore.Event;
import li.strolch.job.ResourceMonitorStateStore.EventType;
import li.strolch.job.ResourceMonitorStateStore.PendingNotification;
import li.strolch.model.Locator;
import li.strolch.model.log.LogMessage;
import li.strolch.model.log.LogSeverity;
import li.strolch.utils.MailAttachment;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.time.Duration;
import java.time.Instant;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;

import static li.strolch.model.Tags.AGENT;
import static org.junit.Assert.*;

public class ResourceMonitorDeliveryTest {

	private static final String REALM = "defaultRealm";
	private static final Instant SAMPLED_AT = Instant.parse("2026-01-02T03:04:05Z");
	private static final Instant BREACH_AT = Instant.parse("2026-01-02T02:00:00Z");
	private static RuntimeMock runtime;
	private static CapturingMailHandler mail;
	private static OperationsLog operationsLog;

	@BeforeClass
	public static void startRuntime() throws Exception {
		Path source = Path.of("target", ResourceMonitorDeliveryTest.class.getSimpleName() + "-source");
		Path config = Files.createDirectories(source.resolve("config"));
		Path minimal = Path.of("src/test/resources/minimaltest/config");
		for (String name : List.of("PrivilegeConfig.xml", "PrivilegeRoles.xml", "PrivilegeUsers.xml"))
			Files.copy(minimal.resolve(name), config.resolve(name), StandardCopyOption.REPLACE_EXISTING);

		String configuration = Files.readString(minimal.resolve("StrolchConfiguration.xml"));
		String components = """
				<Component>
					<name>MailHandler</name>
					<api>li.strolch.handler.mail.MailHandler</api>
					<impl>li.strolch.job.ResourceMonitorDeliveryTest$CapturingMailHandler</impl>
				</Component>
				<Component>
					<name>OperationsLog</name>
					<api>li.strolch.handler.operationslog.OperationsLog</api>
					<impl>li.strolch.handler.operationslog.OperationsLog</impl>
					<depends>RealmHandler</depends>
				</Component>
				""";
		Files.writeString(config.resolve("StrolchConfiguration.xml"), configuration.replace("</env>", components + "</env>"));
		runtime = new RuntimeMock(Path.of("target", ResourceMonitorDeliveryTest.class.getSimpleName()).toAbsolutePath().toString(),
				source.toString()).mockRuntime();
		runtime.startContainer();
		mail = (CapturingMailHandler) runtime.getContainer().getComponent(MailHandler.class);
		operationsLog = runtime.getContainer().getComponent(OperationsLog.class);
	}

	@AfterClass
	public static void stopRuntime() {
		if (runtime != null)
			runtime.destroyRuntime();
	}

	@Before
	public void resetMail() {
		mail.reset();
	}

	@After
	public void clearInterrupt() {
		Thread.interrupted();
	}

	@Test
	public void sendsSignedEncryptedJsonSynchronouslyAndLogsWarning() throws Exception {
		ResourceMonitorJob job = job("warning");
		job.deliverNotification(configuration(List.of("ops@example.org", "admin@example.org")), pending());

		assertEquals(1, mail.calls);
		assertEquals("ops@example.org,admin@example.org", mail.recipients);
		assertEquals("Resource monitor report", mail.subject);
		assertEquals("A resource monitor report is attached.", mail.text);
		assertEquals(1, mail.attachments.length);
		MailAttachment attachment = mail.attachments[0];
		assertTrue(attachment.fileName().startsWith("resource-monitor_"));
		assertEquals("application/json", attachment.type());
		assertTrue(attachment.sign());
		assertTrue(attachment.encrypt());

		JsonObject report = JsonParser.parseString(attachment.attachment()).getAsJsonObject();
		assertEquals(1, report.get("schemaVersion").getAsInt());
		assertEquals("host-monitor-1", report.get("monitorId").getAsString());
		assertEquals("warning", report.get("jobId").getAsString());
		assertEquals(REALM, report.get("realm").getAsString());
		assertEquals(SAMPLED_AT.toString(), report.get("sampledAt").getAsString());
		assertEquals("THRESHOLD", report.get("notificationMode").getAsString());
		JsonArray events = report.getAsJsonArray("events");
		assertEquals(1, events.size());
		JsonObject event = events.get(0).getAsJsonObject();
		assertEquals("disk:/private", event.get("metricKey").getAsString());
		assertEquals("BREACH", event.get("type").getAsString());
		assertEquals(BREACH_AT.toString(), event.get("breachStartedAt").getAsString());
		JsonArray metrics = report.getAsJsonArray("metrics");
		assertEquals(2, metrics.size());
		JsonObject disk = metrics.get(0).getAsJsonObject();
		assertEquals("disk:/private", disk.get("key").getAsString());
		assertEquals("percent", disk.get("unit").getAsString());
		assertEquals(91.5, disk.get("value").getAsDouble(), 0.0);
		assertEquals(80.0, disk.get("threshold").getAsDouble(), 0.0);
		assertEquals("BREACHED", disk.get("status").getAsString());
		assertEquals(1000L, disk.get("totalBytes").getAsLong());
		assertEquals(85L, disk.get("freeBytes").getAsLong());
		assertEquals(75L, disk.get("usableBytes").getAsLong());
		assertEquals(915L, disk.get("usedBytes").getAsLong());
		JsonObject cpu = metrics.get(1).getAsJsonObject();
		assertEquals("processCpu", cpu.get("key").getAsString());
		assertEquals("UNAVAILABLE", cpu.get("status").getAsString());
		for (String key : List.of("value", "totalBytes", "freeBytes", "usableBytes", "usedBytes"))
			assertTrue(key, cpu.has(key) && cpu.get(key).isJsonNull());
		assertFalse(mail.subject.contains("91.5") || mail.text.contains("91.5"));
		assertFalse(mail.subject.contains("disk:/private") || mail.text.contains("disk:/private"));
		assertLog("warning", LogSeverity.Warning);
	}

	@Test
	public void includesRuntimeAndApplicationMetadataInEncryptedJson() throws Exception {
		VersionQueryResult versions = runtime.getAgent().getVersion();
		StrolchVersion applicationVersion = versions.getAppVersion();
		StrolchVersion agentVersion = versions.getAgentVersion();
		String originalApplicationVersion = applicationVersion.getArtifactVersion();
		String originalAgentVersion = agentVersion.getArtifactVersion();
		try {
			applicationVersion.setArtifactVersion("application-test-version");
			agentVersion.setArtifactVersion("strolch-agent-test-version");
			job("metadata").deliverNotification(configuration(List.of("ops@example.org")), pending());

			JsonObject report = JsonParser.parseString(mail.attachments[0].attachment()).getAsJsonObject();
			JsonObject os = report.getAsJsonObject("operatingSystem");
			assertEquals(System.getProperty("os.name"), os.get("name").getAsString());
			assertEquals(System.getProperty("os.version"), os.get("version").getAsString());
			JsonObject jdk = report.getAsJsonObject("jdk");
			assertEquals(System.getProperty("java.vendor"), jdk.get("vendor").getAsString());
			assertEquals(System.getProperty("java.vm.name"), jdk.get("type").getAsString());
			assertEquals(System.getProperty("java.version"), jdk.get("version").getAsString());
			JsonObject application = report.getAsJsonObject("application");
			assertEquals("StrolchRuntimeTest", application.get("name").getAsString());
			assertEquals("dev", application.get("environment").getAsString());
			assertEquals("application-test-version", application.get("version").getAsString());
			assertEquals("strolch-agent-test-version",
					report.getAsJsonObject("strolchAgent").get("version").getAsString());
			assertEquals("Resource monitor report", mail.subject);
			assertEquals("A resource monitor report is attached.", mail.text);
		} finally {
			applicationVersion.setArtifactVersion(originalApplicationVersion);
			agentVersion.setArtifactVersion(originalAgentVersion);
		}
	}

	@Test
	public void representsMissingRuntimeAndApplicationVersionsAsJsonNull() throws Exception {
		VersionQueryResult versions = runtime.getAgent().getVersion();
		StrolchVersion applicationVersion = versions.getAppVersion();
		StrolchVersion agentVersion = versions.getAgentVersion();
		String originalApplicationVersion = applicationVersion.getArtifactVersion();
		String originalAgentVersion = agentVersion.getArtifactVersion();
		try {
			applicationVersion.setArtifactVersion(null);
			agentVersion.setArtifactVersion(null);
			job("missingVersions").deliverNotification(configuration(List.of("ops@example.org")), pending());

			JsonObject report = JsonParser.parseString(mail.attachments[0].attachment()).getAsJsonObject();
			assertTrue(report.getAsJsonObject("application").get("version").isJsonNull());
			assertTrue(report.getAsJsonObject("strolchAgent").get("version").isJsonNull());
		} finally {
			applicationVersion.setArtifactVersion(originalApplicationVersion);
			agentVersion.setArtifactVersion(originalAgentVersion);
		}
	}

	@Test
	public void logsRecoveryAsInformation() throws Exception {
		PendingNotification notification = new PendingNotification("recovery", new Snapshot(SAMPLED_AT,
				List.of(new Metric("cpu", "percent", 10.0, 80.0, Status.HEALTHY, null, null, null, null))),
				NotificationMode.THRESHOLD, List.of(new Event("cpu", EventType.RECOVERY, BREACH_AT)));
		job("recovery").deliverNotification(configuration(List.of("ops@example.org")), notification);
		assertEquals(1, mail.calls);
		assertLog("recovery", LogSeverity.Info);
	}

	@Test
	public void rejectsMissingEncryptionOrSigningBeforeSending() throws Exception {
		for (boolean[] enabled : List.of(new boolean[] {false, true}, new boolean[] {true, false},
				new boolean[] {false, false})) {
			mail.encryptionEnabled = enabled[0];
			mail.signingEnabled = enabled[1];
			try {
				job("disabled").deliverNotification(configuration(List.of("ops@example.org")), pending());
				fail("Expected encryption and signing to be required");
			} catch (IllegalStateException expected) {
				assertTrue(expected.getMessage().contains("encryption and signing"));
			}
			assertEquals(0, mail.calls);
		}
		assertNoReportLog("disabled");
	}

	@Test
	public void rejectsInvalidRecipientsBeforeSending() throws Exception {
		List<List<String>> invalid = Arrays.asList(null, List.of(), Arrays.asList((String) null), List.of(" "),
				List.of("not-an-address"), List.of("a@example.org,b@example.org"), List.of("Team: a@example.org;"),
				List.of("ops@example.org", "bad@@example.org"));
		for (List<String> recipients : invalid) {
			try {
				job("invalid").deliverNotification(configuration(recipients), pending());
				fail("Expected recipients to be rejected: " + recipients);
			} catch (IllegalArgumentException expected) {
				assertNotNull(expected.getMessage());
			}
			assertEquals(0, mail.calls);
		}
		assertNoReportLog("invalid");
	}

	@Test
	public void preservesInterruptWhenSendIsInterrupted() throws Exception {
		mail.interruptOnSend = true;
		try {
			job("interrupted").deliverNotification(configuration(List.of("ops@example.org")), pending());
			fail("Expected interrupted delivery to fail");
		} catch (InterruptedException expected) {
			assertTrue(Thread.currentThread().isInterrupted());
			assertEquals(1, mail.calls);
		}
	}

	@Test
	public void doesNotSendWhenAlreadyInterrupted() throws Exception {
		Thread.currentThread().interrupt();
		try {
			job("alreadyInterrupted").deliverNotification(configuration(List.of("ops@example.org")), pending());
			fail("Expected interrupted delivery to fail");
		} catch (InterruptedException expected) {
			assertTrue(Thread.currentThread().isInterrupted());
			assertEquals(0, mail.calls);
		}
	}

	private static ResourceMonitorJob job(String id) {
		ResourceMonitorJob job = new ResourceMonitorJob(runtime.getAgent(), id, "Resource Monitor", JobMode.Manual);
		job.setSourceRealmName(REALM);
		return job;
	}

	private static Configuration configuration(List<String> recipients) {
		return new Configuration("host-monitor-1", NotificationMode.THRESHOLD, recipients, Duration.ofMinutes(15),
				80.0, Map.of(), 80.0, 80.0, 80.0, 80.0);
	}

	private static PendingNotification pending() {
		Snapshot snapshot = new Snapshot(SAMPLED_AT, List.of(
				new Metric("disk:/private", "percent", 91.5, 80.0, Status.BREACHED, 1000L, 85L, 75L, 915L),
				new Metric("processCpu", "percent", null, 80.0, Status.UNAVAILABLE, null, null, null, null)));
		return new PendingNotification("pending-1", snapshot, NotificationMode.THRESHOLD,
				List.of(new Event("disk:/private", EventType.BREACH, BREACH_AT)));
	}

	private static void assertLog(String id, LogSeverity severity) throws Exception {
		Locator locator = Locator.valueOf(AGENT, ResourceMonitorJob.class.getSimpleName(), id);
		awaitLogQueue();
		List<LogMessage> messages = operationsLog.getMessagesFor(REALM, locator).orElseThrow().stream().toList();
		assertEquals(1, messages.size());
		assertEquals("job.resourceMonitor.report", messages.getFirst().getKey());
		assertEquals(severity, messages.getFirst().getSeverity());
	}

	private static void assertNoReportLog(String id) throws Exception {
		awaitLogQueue();
		Locator locator = Locator.valueOf(AGENT, ResourceMonitorJob.class.getSimpleName(), id);
		assertTrue(operationsLog.getMessagesFor(REALM, locator).isEmpty());
	}

	private static void awaitLogQueue() throws Exception {
		long deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(5);
		while (operationsLog.isQueueNotEmpty() && System.nanoTime() < deadline)
			Thread.sleep(10);
		assertTrue("Operations log queue did not drain", operationsLog.isQueueEmpty());
	}

	public static class CapturingMailHandler extends MailHandler {

		private boolean encryptionEnabled;
		private boolean signingEnabled;
		private boolean interruptOnSend;
		private int calls;
		private String recipients;
		private String subject;
		private String text;
		private MailAttachment[] attachments;

		public CapturingMailHandler(ComponentContainer container, String name) {
			super(container, name);
		}

		void reset() {
			this.encryptionEnabled = true;
			this.signingEnabled = true;
			this.interruptOnSend = false;
			this.calls = 0;
			this.recipients = null;
			this.subject = null;
			this.text = null;
			this.attachments = null;
		}

		@Override
		public boolean isEncryptionEnabled() {
			return this.encryptionEnabled;
		}

		@Override
		public boolean isSigningEnabled() {
			return this.signingEnabled;
		}

		@Override
		public void sendEncryptedMailWithAttachment(String recipients, String subject, String text,
				MailAttachment... attachments) {
			this.calls++;
			this.recipients = recipients;
			this.subject = subject;
			this.text = text;
			this.attachments = attachments;
			if (this.interruptOnSend)
				Thread.currentThread().interrupt();
		}

		@Override
		public void sendMail(String recipients, String subject, String text) {
			throw unexpected();
		}

		@Override
		public void sendEncryptedMail(String recipients, String subject, String text) {
			throw unexpected();
		}

		@Override
		public void sendUnencryptedMail(String recipients, String subject, String text) {
			throw unexpected();
		}

		@Override
		public void sendMailWithAttachment(String recipients, String subject, String text, MailAttachment... attachments) {
			throw unexpected();
		}

		@Override
		public void sendUnencryptedMailWithAttachment(String recipients, String subject, String text,
				MailAttachment... attachments) {
			throw unexpected();
		}

		@Override
		public void sendMailAsync(String recipients, String subject, String text) {
			throw unexpected();
		}

		@Override
		public void sendEncryptedMailAsync(String recipients, String subject, String text) {
			throw unexpected();
		}

		@Override
		public void sendUnencryptedMailAsync(String recipients, String subject, String text) {
			throw unexpected();
		}

		@Override
		public void sendUnencryptedMailWithBodyAsSignedAttachmentIfAvailableAsync(String recipients, String subject,
				String text) {
			throw unexpected();
		}

		@Override
		public void sendUnencryptedMailWithBodyAsSignedAttachmentIfAvailable(String recipients, String subject, String text) {
			throw unexpected();
		}

		@Override
		public void sendMailWithAttachmentAsync(String recipients, String subject, String text, MailAttachment... attachments) {
			throw unexpected();
		}

		@Override
		public void sendEncryptedMailWithAttachmentAsync(String recipients, String subject, String text,
				MailAttachment... attachments) {
			throw unexpected();
		}

		@Override
		public void sendUnencryptedMailWithAttachmentAsync(String recipients, String subject, String text,
				MailAttachment... attachments) {
			throw unexpected();
		}

		@Override
		public String signPlainText(String text) {
			throw unexpected();
		}

		private static AssertionError unexpected() {
			return new AssertionError("Unexpected mail API");
		}
	}
}