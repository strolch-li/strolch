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

import li.strolch.job.ResourceMonitorJob.Configuration;
import li.strolch.job.ResourceMonitorJob.NotificationMode;
import li.strolch.job.ResourceMonitorSampler.DiskReading;
import li.strolch.job.ResourceMonitorSampler.HeapReading;
import li.strolch.job.ResourceMonitorSampler.MemoryReading;
import li.strolch.job.ResourceMonitorSampler.Metric;
import li.strolch.job.ResourceMonitorSampler.MetricsSource;
import li.strolch.job.ResourceMonitorSampler.Snapshot;
import li.strolch.job.ResourceMonitorSampler.Status;
import org.junit.Test;

import java.time.Clock;
import java.time.Duration;
import java.time.Instant;
import java.time.ZoneOffset;
import java.util.List;
import java.util.Map;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

public class ResourceMonitorSamplerTest {

	private static final Instant SAMPLED_AT = Instant.parse("2026-03-01T10:00:00Z");

	@Test
	public void shouldCalculateMetricsApplyRootOverridesAndBreachAtThreshold() {
		MetricsSource source = new FixedMetricsSource(
				List.of(new DiskReading("/", 1000L, 900L, 850L), new DiskReading("/data", 100L, 80L, 70L)),
				new MemoryReading(1000L, 250L), 0.9, 0.49, new HeapReading(80L, 100L));
		Snapshot snapshot = new ResourceMonitorSampler(source, Clock.fixed(SAMPLED_AT, ZoneOffset.UTC)).sample(
				configuration());

		assertEquals(SAMPLED_AT, snapshot.sampledAt());
		Metric root = snapshot.metric("disk:/");
		assertEquals(10.0, root.value(), 0.0001);
		assertEquals(80.0, root.threshold(), 0.0);
		assertEquals(Status.HEALTHY, root.status());
		assertEquals(Long.valueOf(1000), root.totalBytes());
		assertEquals(Long.valueOf(900), root.freeBytes());
		assertEquals(Long.valueOf(850), root.usableBytes());

		Metric data = snapshot.metric("disk:/data");
		assertEquals(20.0, data.value(), 0.0001);
		assertEquals(20.0, data.threshold(), 0.0);
		assertEquals(Status.BREACHED, data.status());
		assertEquals(Status.BREACHED, snapshot.metric("systemMemory").status());
		assertEquals(Status.BREACHED, snapshot.metric("systemCpu").status());
		assertEquals(Double.valueOf(0.9), snapshot.metric("systemCpu").sourceRatio());
		assertEquals(Status.HEALTHY, snapshot.metric("processCpu").status());
		assertEquals(Double.valueOf(0.49), snapshot.metric("processCpu").sourceRatio());
		assertEquals(Status.BREACHED, snapshot.metric("jvmHeap").status());
	}

	@Test
	public void shouldReportInvalidAndUnavailableMeasurementsWithoutTreatingThemAsHealthy() {
		MetricsSource source = new FixedMetricsSource(List.of(new DiskReading("/inaccessible", 0L, 0L, 0L)),
				new MemoryReading(1000L, -1L), -1.0, null, new HeapReading(100L, -1L));
		Snapshot snapshot = new ResourceMonitorSampler(source, Clock.fixed(SAMPLED_AT, ZoneOffset.UTC)).sample(
				configuration());

		assertUnavailable(snapshot.metric("disk:/inaccessible"));
		assertUnavailable(snapshot.metric("systemMemory"));
		assertUnavailable(snapshot.metric("systemCpu"));
		assertEquals(Double.valueOf(-1.0), snapshot.metric("systemCpu").sourceRatio());
		assertUnavailable(snapshot.metric("processCpu"));
		assertNull(snapshot.metric("processCpu").sourceRatio());
		assertUnavailable(snapshot.metric("jvmHeap"));
	}

	@Test
	public void shouldReportUnavailableDiskDiscoveryWhenNoRootsAreReturned() {
		MetricsSource source = new FixedMetricsSource(List.of(), null, null, null, null);
		Snapshot snapshot = new ResourceMonitorSampler(source, Clock.fixed(SAMPLED_AT, ZoneOffset.UTC)).sample(
				configuration());

		assertUnavailable(snapshot.metric("disk:discovery"));
	}

	@Test
	public void shouldReportMissingHeapCountersAsUnavailable() {
		MetricsSource source = new FixedMetricsSource(List.of(), null, null, null, new HeapReading(null, null));
		Snapshot snapshot = new ResourceMonitorSampler(source, Clock.fixed(SAMPLED_AT, ZoneOffset.UTC)).sample(
				configuration());
		assertUnavailable(snapshot.metric("jvmHeap"));
	}

	private static void assertUnavailable(Metric metric) {
		assertEquals(Status.UNAVAILABLE, metric.status());
		assertNull(metric.value());
	}

	private static Configuration configuration() {
		return new Configuration("monitor", NotificationMode.THRESHOLD, List.of("ops@example.org"),
				Duration.ofMinutes(15), 80.0, Map.of("/data", 20.0), 75.0, 90.0, 80.0, 50.0);
	}

	private record FixedMetricsSource(List<DiskReading> disks, MemoryReading physicalMemory, Double systemCpuLoad,
			Double processCpuLoad, HeapReading heap) implements MetricsSource {
	}
}
