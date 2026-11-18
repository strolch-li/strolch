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

import li.strolch.RuntimeMock;
import li.strolch.agent.api.StrolchAgent;
import li.strolch.agent.api.StrolchBootstrapper;
import li.strolch.job.ResourceMonitorJob.Configuration;
import li.strolch.job.ResourceMonitorJob.NotificationMode;
import li.strolch.job.ResourceMonitorSampler.Metric;
import li.strolch.job.ResourceMonitorSampler.Snapshot;
import li.strolch.job.ResourceMonitorSampler.Status;
import li.strolch.persistence.api.StrolchTransaction;
import org.junit.Test;

import java.io.File;
import java.time.Duration;
import java.time.Instant;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.Assert.*;

public class ResourceMonitorStateStoreTest {

	private static final String TARGET_PATH = "target/" + ResourceMonitorStateStoreTest.class.getSimpleName();
	private static final String REALM_NAME = "otherRealm";
	private static final String JOB_ID = "resourceMonitor";
	private static final String MONITOR_ID = "host-monitor-1";
	private static final Instant T0 = Instant.parse("2025-01-01T00:00:00Z");

	@Test
	public void shouldPersistTransitionsAndPendingNotificationsInOrder() throws Exception {
		try (RuntimeMock runtimeMock = new RuntimeMock(new File(TARGET_PATH).getAbsolutePath(),
				"src/test/resources/realmtest").mockRuntime()) {
			runtimeMock.startContainer();
			runtimeMock.getAgent().getContainer().runAsAgent(ctx -> {
				var realm = runtimeMock.getRealm(REALM_NAME);
				var certificate = ctx.certificate();
				var configuration = configuration(NotificationMode.THRESHOLD);
				var store = new ResourceMonitorStateStore();

				ResourceMonitorStateStore.Update breach = store.recordSample(realm, certificate, JOB_ID, configuration,
						snapshot(T0, Status.BREACHED));
				assertEquals(List.of(ResourceMonitorStateStore.EventType.BREACH),
						breach.events().stream().map(ResourceMonitorStateStore.Event::type).toList());
				assertEquals(ResourceMonitorStateStore.DecisionReason.BREACH_EVENT_QUEUED,
						breach.decisions().getFirst().reason());
				assertEquals(1, breach.pending().size());
				String breachNotificationId = breach.pending().getFirst().id();

				ResourceMonitorStateStore reloadedStore = new ResourceMonitorStateStore();
				ResourceMonitorStateStore.Update unavailable = reloadedStore.recordSample(realm, certificate, JOB_ID,
						configuration, snapshot(T0.plusSeconds(10), Status.UNAVAILABLE));
				assertTrue(unavailable.events().isEmpty());
				assertEquals(ResourceMonitorStateStore.DecisionReason.MEASUREMENT_UNAVAILABLE,
						unavailable.decisions().getFirst().reason());
				assertEquals(1, unavailable.pending().size());

				ResourceMonitorStateStore.Update stillBreached = reloadedStore.recordSample(realm, certificate, JOB_ID,
						configuration, snapshot(T0.plusSeconds(30), Status.BREACHED));
				assertTrue(stillBreached.events().isEmpty());
				assertEquals(breachNotificationId, stillBreached.pending().getFirst().id());

				try {
					reloadedStore.acknowledge(realm, certificate, JOB_ID, MONITOR_ID, "not-the-head",
							T0.plusSeconds(30));
					fail("Expected out-of-order acknowledgement to fail");
				} catch (IllegalStateException e) {
					assertTrue(e.getMessage().contains("pending order"));
				}
				reloadedStore.acknowledge(realm, certificate, JOB_ID, MONITOR_ID, breachNotificationId,
						T0.plusSeconds(30));

				ResourceMonitorStateStore.Update tooSoon = reloadedStore.recordSample(realm, certificate, JOB_ID,
						configuration, snapshot(T0.plusSeconds(89), Status.BREACHED));
				assertTrue(tooSoon.events().isEmpty());
				assertTrue(tooSoon.pending().isEmpty());
				assertEquals(ResourceMonitorStateStore.DecisionReason.REMINDER_INTERVAL_NOT_ELAPSED,
						tooSoon.decisions().getFirst().reason());

				ResourceMonitorStateStore.Update reminder = reloadedStore.recordSample(realm, certificate, JOB_ID,
						configuration, snapshot(T0.plusSeconds(90), Status.BREACHED));
				assertEquals(List.of(ResourceMonitorStateStore.EventType.REMINDER),
						reminder.events().stream().map(ResourceMonitorStateStore.Event::type).toList());
				assertEquals(ResourceMonitorStateStore.DecisionReason.REMINDER_EVENT_QUEUED,
						reminder.decisions().getFirst().reason());
				assertEquals(1, reminder.pending().size());
				String reminderNotificationId = reminder.pending().getFirst().id();
				reloadedStore.acknowledge(realm, certificate, JOB_ID, MONITOR_ID, reminderNotificationId,
						T0.plusSeconds(90));

				ResourceMonitorStateStore.Update recovery = reloadedStore.recordSample(realm, certificate, JOB_ID,
						configuration, snapshot(T0.plusSeconds(91), Status.HEALTHY));
				assertEquals(List.of(ResourceMonitorStateStore.EventType.RECOVERY),
						recovery.events().stream().map(ResourceMonitorStateStore.Event::type).toList());
				assertEquals(ResourceMonitorStateStore.DecisionReason.RECOVERY_EVENT_QUEUED,
						recovery.decisions().getFirst().reason());
				assertEquals(1, recovery.pending().size());
				reloadedStore.acknowledge(realm, certificate, JOB_ID, MONITOR_ID, recovery.pending().getFirst().id(),
						T0.plusSeconds(91));

				ResourceMonitorStateStore.Update healthy = reloadedStore.recordSample(realm, certificate, JOB_ID,
						configuration, snapshot(T0.plusSeconds(100), Status.HEALTHY));
				assertTrue(healthy.events().isEmpty());
				assertTrue(healthy.pending().isEmpty());
				assertEquals(ResourceMonitorStateStore.DecisionReason.BELOW_THRESHOLD_NO_ALERT,
						healthy.decisions().getFirst().reason());

				try (StrolchTransaction tx = realm.openTx(certificate, ResourceMonitorStateStoreTest.class, true)) {
					String stateId = ResourceMonitorStateStore.stateResourceId(REALM_NAME, JOB_ID, MONITOR_ID);
					var stateResource = tx.getResourceBy(ResourceMonitorStateStore.STATE_RESOURCE_TYPE, stateId, true);
					assertEquals(JOB_ID, stateResource.getString("jobId"));
					assertEquals(MONITOR_ID, stateResource.getString("monitorId"));
					assertTrue(stateResource.getString("stateJson").contains("\"pending\":[]"));
				}
			});
		}
	}

	@Test
	public void shouldQueueEveryAlwaysModeSnapshotAndIsolateExecutionLocks() throws Exception {
		try (RuntimeMock runtimeMock = new RuntimeMock(new File(TARGET_PATH + "-always").getAbsolutePath(),
				"src/test/resources/realmtest").mockRuntime()) {
			runtimeMock.startContainer();
			runtimeMock.getAgent().getContainer().runAsAgent(ctx -> {
				var realm = runtimeMock.getRealm(REALM_NAME);
				var configuration = configuration(NotificationMode.ALWAYS);
				var store = new ResourceMonitorStateStore();

				ResourceMonitorStateStore.Update first = store.recordSample(realm, ctx.certificate(), JOB_ID,
						configuration, snapshot(T0, Status.HEALTHY));
				ResourceMonitorStateStore.Update second = store.recordSample(realm, ctx.certificate(), JOB_ID,
						configuration, snapshot(T0.plusSeconds(1), Status.HEALTHY));
				assertTrue(first.events().isEmpty());
				assertTrue(second.events().isEmpty());
				assertEquals(ResourceMonitorStateStore.DecisionReason.ALWAYS_MODE_REPORT,
						first.decisions().getFirst().reason());
				assertEquals(ResourceMonitorStateStore.DecisionReason.ALWAYS_MODE_REPORT,
						second.decisions().getFirst().reason());
				assertEquals(1, first.pending().size());
				assertEquals(2, second.pending().size());
				assertSame(ResourceMonitorStateStore.lockFor(REALM_NAME, JOB_ID, MONITOR_ID),
						ResourceMonitorStateStore.lockFor(REALM_NAME, JOB_ID, MONITOR_ID));
				assertNotSame(ResourceMonitorStateStore.lockFor(REALM_NAME, JOB_ID, MONITOR_ID),
						ResourceMonitorStateStore.lockFor(REALM_NAME, "anotherJob", MONITOR_ID));
			});
		}
	}

	@Test
	public void shouldRestorePendingNotificationsAfterPersistentRealmRestart() throws Exception {
		File targetPath = new File("target/" + ResourceMonitorStateStoreTest.class.getSimpleName() + "-restart");
		AtomicReference<String> pendingId = new AtomicReference<>();
		try (RuntimeMock runtimeMock = new RuntimeMock(targetPath.getAbsolutePath(),
				"src/test/resources/realmtest").mockRuntime()) {
			runtimeMock.startContainer();
			runtimeMock.getAgent().getContainer().runAsAgent(ctx -> {
				ResourceMonitorStateStore.Update update = new ResourceMonitorStateStore().recordSample(
						runtimeMock.getRealm("eclipseStore"), ctx.certificate(), JOB_ID,
						configuration(NotificationMode.THRESHOLD), snapshot(T0, Status.BREACHED));
				pendingId.set(update.pending().getFirst().id());
			});
		}

		StrolchAgent restartedAgent = new StrolchBootstrapper(RuntimeMock.getAppVersion()).setupByRoot("dev",
				targetPath.getAbsoluteFile());
		try {
			restartedAgent.initialize();
			restartedAgent.start();
			restartedAgent.getContainer().runAsAgent(ctx -> {
				ResourceMonitorStateStore.Update update = new ResourceMonitorStateStore().recordSample(
						restartedAgent.getRealm("eclipseStore"), ctx.certificate(), JOB_ID,
						configuration(NotificationMode.THRESHOLD), snapshot(T0.plusSeconds(30), Status.BREACHED));
				assertTrue(update.events().isEmpty());
				assertEquals(1, update.pending().size());
				assertEquals(pendingId.get(), update.pending().getFirst().id());
			});
		} finally {
			restartedAgent.stop();
			restartedAgent.destroy();
		}
	}

	private static Configuration configuration(NotificationMode mode) {
		return new Configuration(MONITOR_ID, mode, List.of("ops@example.org"), Duration.ofSeconds(60), 80.0, Map.of(),
				80.0, 80.0, 80.0, 80.0);
	}

	private static Snapshot snapshot(Instant sampledAt, Status status) {
		Double value = switch (status) {
			case BREACHED -> 85.0;
			case HEALTHY -> 40.0;
			case UNAVAILABLE -> null;
		};
		return new Snapshot(sampledAt,
				List.of(new Metric("systemCpu", "percent", value, 80.0, status, null, null, null, null)));
	}
}
