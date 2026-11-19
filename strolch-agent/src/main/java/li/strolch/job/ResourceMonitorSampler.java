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

import com.sun.management.OperatingSystemMXBean;
import li.strolch.job.ResourceMonitorJob.Configuration;

import java.io.File;
import java.lang.management.ManagementFactory;
import java.lang.management.MemoryUsage;
import java.time.Clock;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Objects;

final class ResourceMonitorSampler {

	private final MetricsSource source;
	private final Clock clock;

	ResourceMonitorSampler() {
		this(new SystemMetricsSource(), Clock.systemUTC());
	}

	ResourceMonitorSampler(MetricsSource source, Clock clock) {
		this.source = Objects.requireNonNull(source);
		this.clock = Objects.requireNonNull(clock);
	}

	Snapshot sample(Configuration configuration) {
		return sample(configuration, this.clock.instant());
	}

	Snapshot sample(Configuration configuration, Instant sampledAt) {
		List<Metric> metrics = new ArrayList<>();
		List<DiskReading> disks = this.source.disks();
		if (disks == null || disks.isEmpty()) {
			metrics.add(unavailable("disk:discovery", configuration.diskThresholdPercent(), null, null, null, null));
		} else {
			for (DiskReading disk : disks) {
				String rootPath = disk == null ? null : disk.rootPath();
				String key = rootPath == null ? "disk:unknown" : "disk:" + rootPath;
				double threshold = rootPath == null ? configuration.diskThresholdPercent() :
						configuration.diskThresholdFor(rootPath);
				if (disk == null) {
					metrics.add(unavailable(key, threshold, null, null, null, null));
				} else {
					metrics.add(
							percentFromBytes(key, threshold, disk.totalBytes(), disk.freeBytes(), disk.usableBytes()));
				}
			}
		}

		MemoryReading memory = this.source.physicalMemory();
		metrics.add(memory == null ?
				unavailable("systemMemory", configuration.systemMemoryThresholdPercent(), null, null, null, null) :
				percentFromBytes("systemMemory", configuration.systemMemoryThresholdPercent(), memory.totalBytes(),
						memory.freeBytes(), null));

		metrics.add(
				percentFromRatio("systemCpu", configuration.systemCpuThresholdPercent(), this.source.systemCpuLoad()));
		metrics.add(percentFromRatio("processCpu", configuration.processCpuThresholdPercent(),
				this.source.processCpuLoad()));

		HeapReading heap = this.source.heap();
		if (heap == null
				|| heap.maxBytes() == null
				|| heap.usedBytes() == null
				|| heap.maxBytes() <= 0
				|| heap.usedBytes() < 0
				|| heap.usedBytes() > heap.maxBytes()) {
			metrics.add(unavailable("jvmHeap", configuration.jvmHeapThresholdPercent(),
					heap == null ? null : heap.usedBytes(), heap == null ? null : heap.maxBytes(), null, null));
		} else {
			long freeBytes = heap.maxBytes() - heap.usedBytes();
			metrics.add(toMetric("jvmHeap", configuration.jvmHeapThresholdPercent(),
					heap.usedBytes() * 100.0 / heap.maxBytes(), heap.maxBytes(), freeBytes, null, heap.usedBytes()));
		}

		metrics.sort(Comparator.comparing(Metric::key));
		return new Snapshot(sampledAt, List.copyOf(metrics));
	}

	private static Metric percentFromBytes(String key, double threshold, Long totalBytes, Long freeBytes,
			Long usableBytes) {
		if (totalBytes == null
				|| freeBytes == null
				|| totalBytes <= 0
				|| freeBytes < 0
				|| freeBytes > totalBytes
				|| usableBytes != null && usableBytes < 0)
			return unavailable(key, threshold, null, totalBytes, freeBytes, usableBytes);

		long usedBytes = totalBytes - freeBytes;
		return toMetric(key, threshold, usedBytes * 100.0 / totalBytes, totalBytes, freeBytes, usableBytes, usedBytes);
	}

	private static Metric percentFromRatio(String key, double threshold, Double ratio) {
		if (ratio == null || !Double.isFinite(ratio) || ratio < 0.0 || ratio > 1.0)
			return unavailable(key, threshold, null, null, null, null, ratio);
		return toMetric(key, threshold, ratio * 100.0, null, null, null, null, ratio);
	}

	private static Metric toMetric(String key, double threshold, double value, Long totalBytes, Long freeBytes,
			Long usableBytes, Long usedBytes) {
		return toMetric(key, threshold, value, totalBytes, freeBytes, usableBytes, usedBytes, null);
	}

	private static Metric toMetric(String key, double threshold, double value, Long totalBytes, Long freeBytes,
			Long usableBytes, Long usedBytes, Double sourceRatio) {
		Status status = value >= threshold ? Status.BREACHED : Status.HEALTHY;
		return new Metric(key, "percent", value, threshold, status, totalBytes, freeBytes, usableBytes, usedBytes,
				sourceRatio);
	}

	private static Metric unavailable(String key, double threshold, Long usedBytes, Long totalBytes, Long freeBytes,
			Long usableBytes) {
		return unavailable(key, threshold, usedBytes, totalBytes, freeBytes, usableBytes, null);
	}

	private static Metric unavailable(String key, double threshold, Long usedBytes, Long totalBytes, Long freeBytes,
			Long usableBytes, Double sourceRatio) {
		return new Metric(key, "percent", null, threshold, Status.UNAVAILABLE, totalBytes, freeBytes, usableBytes,
				usedBytes, sourceRatio);
	}

	enum Status {
		HEALTHY,
		BREACHED,
		UNAVAILABLE
	}

	record Metric(String key, String unit, Double value, double threshold, Status status, Long totalBytes,
			Long freeBytes, Long usableBytes, Long usedBytes, Double sourceRatio) {

		Metric(String key, String unit, Double value, double threshold, Status status, Long totalBytes, Long freeBytes,
				Long usableBytes, Long usedBytes) {
			this(key, unit, value, threshold, status, totalBytes, freeBytes, usableBytes, usedBytes, null);
		}
	}

	record Snapshot(Instant sampledAt, List<Metric> metrics) {
		Metric metric(String key) {
			return this.metrics
					.stream()
					.filter(metric -> metric.key().equals(key))
					.findFirst()
					.orElseThrow(() -> new IllegalArgumentException("No metric found for key " + key));
		}
	}

	interface MetricsSource {
		List<DiskReading> disks();

		MemoryReading physicalMemory();

		Double systemCpuLoad();

		Double processCpuLoad();

		HeapReading heap();
	}

	record DiskReading(String rootPath, Long totalBytes, Long freeBytes, Long usableBytes) {
	}

	record MemoryReading(Long totalBytes, Long freeBytes) {
	}

	record HeapReading(Long usedBytes, Long maxBytes) {
	}

	private static class SystemMetricsSource implements MetricsSource {

		private final OperatingSystemMXBean operatingSystemBean;

		private SystemMetricsSource() {
			java.lang.management.OperatingSystemMXBean bean = ManagementFactory.getOperatingSystemMXBean();
			this.operatingSystemBean = bean instanceof OperatingSystemMXBean osBean ? osBean : null;
		}

		@Override
		public List<DiskReading> disks() {
			File[] roots;
			try {
				roots = File.listRoots();
			} catch (SecurityException e) {
				return List.of(new DiskReading(null, null, null, null));
			}
			if (roots == null || roots.length == 0)
				return List.of();

			List<DiskReading> readings = new ArrayList<>(roots.length);
			for (File root : roots) {
				try {
					readings.add(new DiskReading(root.getPath(), root.getTotalSpace(), root.getFreeSpace(),
							root.getUsableSpace()));
				} catch (SecurityException e) {
					readings.add(new DiskReading(root.getPath(), null, null, null));
				}
			}
			return readings;
		}

		@Override
		public MemoryReading physicalMemory() {
			if (this.operatingSystemBean == null)
				return null;
			try {
				return new MemoryReading(this.operatingSystemBean.getTotalMemorySize(),
						this.operatingSystemBean.getFreeMemorySize());
			} catch (RuntimeException e) {
				return null;
			}
		}

		@Override
		public Double systemCpuLoad() {
			if (this.operatingSystemBean == null)
				return null;
			try {
				return this.operatingSystemBean.getCpuLoad();
			} catch (RuntimeException e) {
				return null;
			}
		}

		@Override
		public Double processCpuLoad() {
			if (this.operatingSystemBean == null)
				return null;
			try {
				return this.operatingSystemBean.getProcessCpuLoad();
			} catch (RuntimeException e) {
				return null;
			}
		}

		@Override
		public HeapReading heap() {
			try {
				MemoryUsage heap = ManagementFactory.getMemoryMXBean().getHeapMemoryUsage();
				return new HeapReading(heap.getUsed(), heap.getMax());
			} catch (RuntimeException e) {
				return null;
			}
		}
	}
}
