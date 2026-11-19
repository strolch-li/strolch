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

import ch.qos.logback.classic.Level;
import ch.qos.logback.classic.Logger;
import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.core.read.ListAppender;
import li.strolch.RuntimeMock;
import li.strolch.agent.api.StrolchAgent;
import li.strolch.model.ParameterBag;
import li.strolch.model.Resource;
import li.strolch.model.parameter.BooleanParameter;
import li.strolch.model.parameter.StringListParameter;
import li.strolch.model.parameter.StringParameter;
import li.strolch.persistence.api.StrolchTransaction;
import org.junit.Test;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.time.Clock;
import java.time.Instant;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

import static li.strolch.runtime.StrolchConstants.TYPE_STROLCH_JOB;
import static org.junit.Assert.*;

public class ResourceMonitorJobTest {

	@Test
	public void shouldLoadValidatedConfigurationFromModelSourceRealm() throws Exception {
		File targetPath = new File("target/" + ResourceMonitorJobTest.class.getSimpleName());
		try (RuntimeMock runtimeMock = new RuntimeMock(targetPath.getAbsolutePath(),
				"src/test/resources/realmtest").mockRuntime()) {
			runtimeMock.startContainer();
			StrolchAgent agent = runtimeMock.getAgent();
			StrolchJobsHandler jobsHandler = new StrolchJobsHandler(agent.getContainer(), "StrolchJobsHandler");
			AtomicReference<ResourceMonitorJob> loadedJob = new AtomicReference<>();
			AtomicReference<ResourceMonitorJob.Configuration> configuration = new AtomicReference<>();

			agent.getContainer().runAsAgent(ctx -> {
				try (StrolchTransaction tx = runtimeMock
						.getRealm("otherRealm")
						.openTx(ctx.certificate(), ResourceMonitorJobTest.class, false)) {
					tx.add(validJobResource());
					tx.commitOnClose();
				}

				List<StrolchJob> jobs = new ArrayList<>();
				try (StrolchTransaction tx = runtimeMock
						.getRealm("otherRealm")
						.openTx(ctx.certificate(), ResourceMonitorJobTest.class, true)) {
					jobsHandler.loadJob(jobs, tx.getResourceBy(TYPE_STROLCH_JOB, "resourceMonitor", true), "otherRealm",
							false);
				}

				assertEquals(1, jobs.size());
				ResourceMonitorJob job = (ResourceMonitorJob) jobs.getFirst();
				loadedJob.set(job);
				configuration.set(job.readConfiguration(ctx.certificate()));
			});

			assertEquals("otherRealm", loadedJob.get().getSourceRealmName());
			assertEquals("host-monitor-1", configuration.get().monitorId());
			assertEquals(ResourceMonitorJob.NotificationMode.THRESHOLD, configuration.get().notificationMode());
			assertEquals(List.of("ops@example.org"), configuration.get().recipients());
			assertEquals(15.0, configuration.get().diskThresholdFor("/data"), 0.0);
			assertEquals(80.0, configuration.get().diskThresholdFor("/"), 0.0);
		}
	}

	@Test
	public void shouldRejectThresholdOutsideConfiguredRange() {
		Resource resource = validJobResource();
		resource.getParameter("diskThresholdPercent", true).setValue("0");

		try {
			ResourceMonitorJob.Configuration.from(resource);
			fail("Expected invalid threshold to be rejected");
		} catch (IllegalArgumentException e) {
			assertTrue(e.getMessage().contains("diskThresholdPercent"));
		}
	}

	@Test
	public void shouldRejectMalformedDiskOverrides() {
		for (String json : List.of("{'/':90}", "{/:90}", "{\"/\":90,}", "{\"/\":90} {}")) {
			Resource resource = validJobResource();
			resource.setString("diskThresholdOverrides", json);
			try {
				ResourceMonitorJob.Configuration.from(resource);
				fail("Expected invalid JSON to be rejected: " + json);
			} catch (IllegalArgumentException expected) {
				assertTrue(expected.getMessage().contains("diskThresholdOverrides"));
			}
		}
	}

	@Test
	public void shouldRetryUnacknowledgedDeliveryAfterJobReload() throws Exception {
		File targetPath = new File("target/" + ResourceMonitorJobTest.class.getSimpleName() + "-retry");
		try (RuntimeMock runtimeMock = new RuntimeMock(targetPath.getAbsolutePath(),
				"src/test/resources/realmtest").mockRuntime()) {
			runtimeMock.startContainer();
			StrolchAgent agent = runtimeMock.getAgent();
			List<String> deliveries = new ArrayList<>();
			agent.getContainer().runAsAgent(ctx -> {
				Resource resource = validJobResource();
				resource.setString("notificationMode", "ALWAYS");
				try (StrolchTransaction tx = runtimeMock
						.getRealm("otherRealm")
						.openTx(ctx.certificate(), ResourceMonitorJobTest.class, false)
						.rollbackOnFailure()) {
					tx.add(resource);
					tx.commitOnClose();
				}

				TestMonitor failed = new TestMonitor(agent, deliveries, true);
				failed.setSourceRealmName("otherRealm");
				try {
					failed.execute(ctx);
					fail("Expected delivery failure");
				} catch (IllegalStateException expected) {
					assertEquals("Simulated delivery failure", expected.getMessage());
				}

				TestMonitor reloaded = new TestMonitor(agent, deliveries, false);
				reloaded.setSourceRealmName("otherRealm");
				reloaded.execute(ctx);
				assertEquals(3, deliveries.size());
				assertEquals(deliveries.get(0), deliveries.get(1));
				assertNotEquals(deliveries.get(1), deliveries.get(2));
			});
		}
	}

	private static class TestMonitor extends ResourceMonitorJob {

		private final List<String> deliveries;
		private final boolean fail;

		TestMonitor(StrolchAgent agent, List<String> deliveries, boolean fail) {
			super(agent, "resourceMonitor", "Resource Monitor", JobMode.Manual);
			this.deliveries = deliveries;
			this.fail = fail;
		}

		@Override
		void deliverNotification(Configuration configuration, ResourceMonitorStateStore.PendingNotification pending) {
			this.deliveries.add(pending.id());
			if (this.fail)
				throw new IllegalStateException("Simulated delivery failure");
		}
	}

	@Test
	public void shouldSerializeOverlappingReloadedJobInstances() throws Exception {
		File targetPath = new File("target/" + ResourceMonitorJobTest.class.getSimpleName() + "-overlap");
		try (RuntimeMock runtimeMock = new RuntimeMock(targetPath.getAbsolutePath(),
				"src/test/resources/realmtest").mockRuntime()) {
			runtimeMock.startContainer();
			StrolchAgent agent = runtimeMock.getAgent();
			agent.getContainer().runAsAgent(ctx -> {
				Resource resource = validJobResource();
				resource.setString("notificationMode", "ALWAYS");
				try (StrolchTransaction tx = runtimeMock
						.getRealm("otherRealm")
						.openTx(ctx.certificate(), ResourceMonitorJobTest.class, false)
						.rollbackOnFailure()) {
					tx.add(resource);
					tx.commitOnClose();
				}

				CountDownLatch firstEntered = new CountDownLatch(1);
				CountDownLatch secondStarted = new CountDownLatch(1);
				CountDownLatch release = new CountDownLatch(1);
				AtomicInteger deliveries = new AtomicInteger();
				ResourceMonitorJob first = blockingMonitor(agent, firstEntered, release, deliveries);
				ResourceMonitorJob second = blockingMonitor(agent, firstEntered, release, deliveries);
				try (var executor = Executors.newFixedThreadPool(2)) {
					var firstResult = executor.submit(() -> {
						first.execute(ctx);
						return null;
					});
					try {
						assertTrue(firstEntered.await(10, TimeUnit.SECONDS));
						var secondResult = executor.submit(() -> {
							secondStarted.countDown();
							second.execute(ctx);
							return null;
						});
						assertTrue(secondStarted.await(10, TimeUnit.SECONDS));
						Thread.sleep(200);
						assertEquals(1, deliveries.get());
						release.countDown();
						firstResult.get(10, TimeUnit.SECONDS);
						secondResult.get(10, TimeUnit.SECONDS);
						assertEquals(2, deliveries.get());
					} finally {
						release.countDown();
					}
				}
			});
		}
	}

	private static ResourceMonitorJob blockingMonitor(StrolchAgent agent, CountDownLatch entered,
			CountDownLatch release, AtomicInteger deliveries) {
		ResourceMonitorJob monitor = new ResourceMonitorJob(agent, "resourceMonitor", "Resource Monitor",
				JobMode.Manual) {
			@Override
			void deliverNotification(Configuration configuration, ResourceMonitorStateStore.PendingNotification pending)
					throws Exception {
				deliveries.incrementAndGet();
				entered.countDown();
				if (!release.await(10, TimeUnit.SECONDS))
					throw new IllegalStateException("Timed out waiting for the first delivery");
			}
		};
		monitor.setSourceRealmName("otherRealm");
		return monitor;
	}

	@Test
	public void shouldRejectMalformedDiskThresholdOverrides() {
		Resource resource = validJobResource();
		resource.getParameter("diskThresholdOverrides", true).setValue("[70]");

		try {
			ResourceMonitorJob.Configuration.from(resource);
			fail("Expected malformed disk overrides to be rejected");
		} catch (IllegalArgumentException e) {
			assertTrue(e.getMessage().contains("diskThresholdOverrides"));
		}
	}

	@Test
	public void shouldDefaultVerboseToFalseAndReadConfiguredValue() {
		Resource resource = validJobResource();
		assertFalse(ResourceMonitorJob.Configuration.from(resource).verbose());

		resource.addParameter(new BooleanParameter("verbose", "Verbose", false));
		assertFalse(ResourceMonitorJob.Configuration.from(resource).verbose());
		resource.getParameter("verbose", true).setValue(true);
		assertTrue(ResourceMonitorJob.Configuration.from(resource).verbose());
	}

	@Test
	public void shouldLogVerboseMetricReasonsAndSilenceLogsWhenDisabled() throws Exception {
		File targetPath = new File("target/" + ResourceMonitorJobTest.class.getSimpleName() + "-verbose");
		try (RuntimeMock runtimeMock = new RuntimeMock(targetPath.getAbsolutePath(),
				"src/test/resources/realmtest").mockRuntime()) {
			runtimeMock.startContainer();
			Logger logger = (Logger) LoggerFactory.getLogger(ResourceMonitorJob.class);
			Level previousLevel = logger.getLevel();
			ListAppender<ILoggingEvent> appender = new ListAppender<>();
			appender.start();
			logger.setLevel(Level.INFO);
			logger.addAppender(appender);
			try {
				runtimeMock.getAgent().getContainer().runAsAgent(ctx -> {
					Resource resource = validJobResource();
					resource.addParameter(new BooleanParameter("verbose", "Verbose", true));
					try (StrolchTransaction tx = runtimeMock
							.getRealm("otherRealm")
							.openTx(ctx.certificate(), ResourceMonitorJobTest.class, false)
							.rollbackOnFailure()) {
						tx.add(resource);
						tx.commitOnClose();
					}

					ResourceMonitorJob monitor = new LoggingMonitor(runtimeMock.getAgent(), false);
					monitor.setSourceRealmName("otherRealm");
					monitor.execute(ctx);
					List<String> firstRun = appender.list.stream().map(ILoggingEvent::getFormattedMessage).toList();
					assertTrue(firstRun
							.stream()
							.anyMatch(message -> message.contains("metric disk:/unavailable") && message.contains(
									"measurement unavailable") && message.contains(
									"inputs=usedBytes=unavailable, freeBytes=-1, totalBytes=0, usableBytes=-1")));
					assertTrue(firstRun
							.stream()
							.anyMatch(message -> message.contains("metric disk:/test") && message.contains(
									"inputs=usedBytes=150, freeBytes=50, totalBytes=200, usableBytes=25")));
					assertTrue(firstRun
							.stream()
							.anyMatch(message -> message.contains("metric systemMemory") && message.contains(
									"inputs=usedBytes=20, freeBytes=80, totalBytes=100")));
					assertTrue(firstRun
							.stream()
							.anyMatch(message -> message.contains("metric jvmHeap") && message.contains(
									"inputs=usedBytes=20, freeBytes=80, maxBytes=100")));
					assertTrue(firstRun
							.stream()
							.anyMatch(message -> message.contains("metric systemCpu") && message.contains(
									"inputs=sourceRatio=0.95") && message.contains("breach event queued")));
					assertTrue(firstRun
							.stream()
							.anyMatch(message -> message.contains("metric processCpu") && message.contains(
									"status=UNAVAILABLE") && message.contains("inputs=sourceRatio=-0.25")));
					assertTrue(firstRun
							.stream()
							.anyMatch(message -> message.contains("delivered and acknowledged report")));

					appender.list.clear();
					monitor.execute(ctx);
					List<String> secondRun = appender.list.stream().map(ILoggingEvent::getFormattedMessage).toList();
					assertTrue(secondRun
							.stream()
							.anyMatch(message -> message.contains("reminder interval has not elapsed")));
					assertTrue(secondRun.stream().anyMatch(message -> message.contains("did not queue a new report")));

					try (StrolchTransaction tx = runtimeMock
							.getRealm("otherRealm")
							.openTx(ctx.certificate(), ResourceMonitorJobTest.class, false)
							.rollbackOnFailure()) {
						Resource job = tx.readLock(tx.getResourceBy(TYPE_STROLCH_JOB, "resourceMonitor", true));
						job.setBoolean("verbose", false);
						tx.update(job);
						tx.commitOnClose();
					}
					appender.list.clear();
					monitor.execute(ctx);
					assertTrue("Verbose=false must suppress monitor status logs", appender.list.isEmpty());
				});
			} finally {
				logger.detachAppender(appender);
				appender.stop();
				logger.setLevel(previousLevel);
			}
		}
	}

	@Test
	public void shouldLogDeliveryFailureAndPendingRetryWhenVerbose() throws Exception {
		File targetPath = new File("target/" + ResourceMonitorJobTest.class.getSimpleName() + "-verbose-retry");
		try (RuntimeMock runtimeMock = new RuntimeMock(targetPath.getAbsolutePath(),
				"src/test/resources/realmtest").mockRuntime()) {
			runtimeMock.startContainer();
			Logger logger = (Logger) LoggerFactory.getLogger(ResourceMonitorJob.class);
			Level previousLevel = logger.getLevel();
			ListAppender<ILoggingEvent> appender = new ListAppender<>();
			appender.start();
			logger.setLevel(Level.INFO);
			logger.addAppender(appender);
			try {
				runtimeMock.getAgent().getContainer().runAsAgent(ctx -> {
					Resource resource = validJobResource();
					resource.setString("notificationMode", "ALWAYS");
					resource.addParameter(new BooleanParameter("verbose", "Verbose", true));
					try (StrolchTransaction tx = runtimeMock
							.getRealm("otherRealm")
							.openTx(ctx.certificate(), ResourceMonitorJobTest.class, false)
							.rollbackOnFailure()) {
						tx.add(resource);
						tx.commitOnClose();
					}

					LoggingMonitor monitor = new LoggingMonitor(runtimeMock.getAgent(), true);
					monitor.setSourceRealmName("otherRealm");
					try {
						monitor.execute(ctx);
						fail("Expected simulated delivery failure");
					} catch (IllegalStateException expected) {
						assertEquals("Simulated delivery failure", expected.getMessage());
					}
					assertTrue(appender.list
							.stream()
							.map(ILoggingEvent::getFormattedMessage)
							.anyMatch(message -> message.contains("delivery failed") && message.contains(
									"remains pending")));

					appender.list.clear();
					monitor.fail = false;
					monitor.execute(ctx);
					List<String> retryLogs = appender.list.stream().map(ILoggingEvent::getFormattedMessage).toList();
					assertTrue(retryLogs.stream().anyMatch(message -> message.contains("retrying report")));
					assertTrue(retryLogs
							.stream()
							.anyMatch(message -> message.contains("delivered and acknowledged report")));
				});
			} finally {
				logger.detachAppender(appender);
				appender.stop();
				logger.setLevel(previousLevel);
			}
		}
	}

	private static class LoggingMonitor extends ResourceMonitorJob {

		private boolean fail;

		LoggingMonitor(StrolchAgent agent, boolean fail) {
			super(agent, "resourceMonitor", "Resource Monitor", JobMode.Manual,
					new ResourceMonitorSampler(new ResourceMonitorSampler.MetricsSource() {
						@Override
						public List<ResourceMonitorSampler.DiskReading> disks() {
							return List.of(new ResourceMonitorSampler.DiskReading("/test", 200L, 50L, 25L),
									new ResourceMonitorSampler.DiskReading("/unavailable", 0L, -1L, -1L));
						}

						@Override
						public ResourceMonitorSampler.MemoryReading physicalMemory() {
							return new ResourceMonitorSampler.MemoryReading(100L, 80L);
						}

						@Override
						public Double systemCpuLoad() {
							return 0.95;
						}

						@Override
						public Double processCpuLoad() {
							return -0.25;
						}

						@Override
						public ResourceMonitorSampler.HeapReading heap() {
							return new ResourceMonitorSampler.HeapReading(20L, 100L);
						}
					}, Clock.fixed(Instant.now(), ZoneOffset.UTC)));
			this.fail = fail;
		}

		@Override
		void deliverNotification(Configuration configuration, ResourceMonitorStateStore.PendingNotification pending) {
			if (this.fail)
				throw new IllegalStateException("Simulated delivery failure");
		}
	}

	private static Resource validJobResource() {
		Resource job = new Resource("resourceMonitor", "Resource Monitor", TYPE_STROLCH_JOB);
		job.addParameterBag(new ParameterBag("parameters", "Parameters", "Parameters"));
		job.addParameter(new StringParameter("className", "Class Name", ResourceMonitorJob.class.getName()));
		job.addParameter(new StringParameter("mode", "Job Mode", JobMode.Manual.name()));
		job.addParameter(new StringParameter("monitorId", "Monitor ID", "host-monitor-1"));
		job.addParameter(new StringParameter("notificationMode", "Notification Mode", "THRESHOLD"));
		job.addParameter(new StringListParameter("recipients", "Recipients", List.of("ops@example.org")));
		job.addParameter(new StringParameter("reminderInterval", "Reminder Interval", "PT15M"));
		job.addParameter(new StringParameter("diskThresholdPercent", "Disk Threshold", "80"));
		job.addParameter(new StringParameter("diskThresholdOverrides", "Disk Overrides", "{\"/data\":15}"));
		job.addParameter(new StringParameter("systemMemoryThresholdPercent", "System Memory Threshold", "85"));
		job.addParameter(new StringParameter("systemCpuThresholdPercent", "System CPU Threshold", "90"));
		job.addParameter(new StringParameter("jvmHeapThresholdPercent", "JVM Heap Threshold", "85"));
		job.addParameter(new StringParameter("processCpuThresholdPercent", "Process CPU Threshold", "90"));
		return job;
	}
}
