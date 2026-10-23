/*
 * Copyright (c) 2015-2025 Robert von Burg <eitch@eitchnet.ch>
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
package li.strolch.test.benchmark;

import li.strolch.agent.api.StrolchRealm;
import li.strolch.agent.impl.DataStoreMode;
import li.strolch.model.Resource;
import li.strolch.model.Order;
import li.strolch.model.StrolchRootElement;
import li.strolch.model.activity.Activity;
import li.strolch.model.activity.TimeOrdering;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Certificate;
import li.strolch.search.ResourceSearch;
import li.strolch.testbase.runtime.RuntimeMock;
import org.openjdk.jmh.annotations.*;
import org.openjdk.jmh.infra.Blackhole;
import org.openjdk.jmh.infra.ThreadParams;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.UUID;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.CopyOnWriteArrayList;

/** Shared-realm benchmark: a different root per worker prevents artificial same-root contention. */
@State(Scope.Benchmark)
@BenchmarkMode(Mode.Throughput)
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 2)
@Measurement(iterations = 5, time = 2)
@Fork(3)
public class TransientTransactionBenchmark {

	@Param({"1000", "100000", "1000000"})
	public int roots;

	@Param({"1"})
	public int types;

	@Param({"0"})
	public long workTokens;

	@Param({"uniform"})
	public String dataset;

	@Param({"Resource"})
	public String rootKind;

	@Param({"BenchType0"})
	public String targetType;

	private RuntimeMock runtime;
	private StrolchRealm realm;
	private Certificate certificate;
	private final CopyOnWriteArrayList<Worker> workers = new CopyOnWriteArrayList<>();

	@Setup(Level.Trial)
	public void setup() {
		if (roots < 64 || types < 1 || types > roots)
			throw new IllegalArgumentException("Require roots >= 64 and 1 <= types <= roots");
		String target = Path.of("target", "isolation-benchmark", UUID.randomUUID().toString()).toString();
		this.runtime = new RuntimeMock().mockRuntime(target, "src/runtime_isolation");
		this.runtime.startContainer();
		this.certificate = this.runtime.loginTest();
		this.realm = this.runtime.getContainer().getRealm(this.certificate);
		if (this.realm.getMode() != DataStoreMode.TRANSIENT)
			throw new IllegalStateException("Benchmark must exercise TRANSIENT, not EMPTY");
		System.out.println("MODEL_IMPLEMENTATION=" + this.realm.getClass().getProtectionDomain().getCodeSource().getLocation());
		if (dataset.equals("uniform")) {
			for (int offset = 0; offset < roots; offset += 10000) {
				try (StrolchTransaction tx = open(false)) {
					var batch = new ArrayList<Resource>();
					for (int i = offset; i < Math.min(roots, offset + 10000); i++) {
						Resource resource = new Resource("r" + i, "Product " + i, "BenchType" + (i % types));
						parameters(resource, i);
						batch.add(resource);
					}
					tx.getResourceMap().addAll(tx, batch);
					tx.commitOnClose();
				}
			}
		} else {
			int multiplier = switch (dataset) {
				case "customer" -> 1;
				case "customer-double" -> 2;
				default -> throw new IllegalArgumentException("Unknown dataset: " + dataset);
			};
			loadCustomer(multiplier);
		}
		try (StrolchTransaction tx = open(true)) {
			long total = tx.getResourceCount() + tx.getOrderCount() + tx.getActivityCount();
			if (total != roots)
				throw new AssertionError("Fixture contains " + total + " roots, requested " + roots);
		}

	}

	private static void parameters(StrolchRootElement root, int index) {
		root.setInteger("revision", 0);
		root.setString("description", "Synthetic benchmark object");
		root.setString("sku", "sku" + index);
	}

	private void loadCustomer(int multiplier) {
		String[] resourceTypes = {"BIN_LOCATION_MAP", "Configuration", "Enumeration", "Gateway", "Inf2Prod",
				"Infusion", "Location", "Migrations", "Notification", "Person", "PersonPicking", "PickingItem",
				"PickingList", "Product", "ProductStock", "Purpose", "Report", "Section", "Slot", "Storage",
				"StrolchJob", "SystemBoard", "SystemBoardOverrides", "Template", "Unit", "User"};
		int[] resourceCounts = {101, 2, 4, 14, 1, 11, 18, 1, 1, 62905, 8523, 45264, 5487, 6802, 81,
				18, 21, 520, 11477, 157, 2, 12, 14, 14, 1, 1760};
		String[] orderTypes = {"FromStock", "Inventory", "StockOrder", "Template", "ToStock"};
		int[] orderCounts = {112787, 15273, 25, 6, 18891};
		for (int t = 0; t < resourceTypes.length; t++) {
			int count = resourceCounts[t] * multiplier;
			for (int offset = 0; offset < count; offset += 10000) {
				try (StrolchTransaction tx = open(false)) {
					var batch = new ArrayList<Resource>();
					for (int i = offset; i < Math.min(count, offset + 10000); i++) {
						Resource root = new Resource("r" + i, "Synthetic " + i, resourceTypes[t]);
						parameters(root, i);
						batch.add(root);
					}
					tx.getResourceMap().addAll(tx, batch);
					tx.commitOnClose();
				}
			}
		}
		for (int t = 0; t < orderTypes.length; t++) {
			int count = orderCounts[t] * multiplier;
			for (int offset = 0; offset < count; offset += 10000) {
				try (StrolchTransaction tx = open(false)) {
					var batch = new ArrayList<Order>();
					for (int i = offset; i < Math.min(count, offset + 10000); i++) {
						Order root = new Order("r" + i, "Synthetic " + i, orderTypes[t]);
						parameters(root, i);
						batch.add(root);
					}
					tx.getOrderMap().addAll(tx, batch);
					tx.commitOnClose();
				}
			}
		}
		try (StrolchTransaction tx = open(false)) {
			var activities = new ArrayList<Activity>();
			// The supplied counts say only "a few" Activities: use ten synthetic roots explicitly.
			for (int i = 0; i < 10 * multiplier; i++)
				activities.add(new Activity("a" + i, "Synthetic activity", "BenchmarkActivity", TimeOrdering.SERIES));
			tx.getActivityMap().addAll(tx, activities);
			tx.commitOnClose();
		}
	}

	@TearDown(Level.Trial)
	public void tearDown() {
		try {
			// JMH's iteration barrier has stopped all workers. Verify here, before shared-state teardown.
			for (Worker worker : this.workers) {
				int revision = read(worker);
				if (revision != worker.successfulUpdates)
					throw new AssertionError("Stored revision " + revision + " != successful commits " + worker.successfulUpdates);
			}
		} finally {
			if (this.runtime != null)
				this.runtime.destroyRuntime();
		}
	}

	@State(Scope.Thread)
	public static class Worker {
		private String id;
		private String type;
		private String kind;
		private long sequence;
		private long successfulUpdates;

		@Setup(Level.Trial)
		public void setup(ThreadParams params, TransientTransactionBenchmark model) {
			int index = params.getThreadIndex();
			if (index >= model.roots)
				throw new IllegalArgumentException("Each worker needs its own root");
			this.id = "r" + index;
			this.type = model.dataset.equals("uniform") ? "BenchType" + (index % model.types) : model.targetType;
			this.kind = model.rootKind;
			model.workers.add(this);
		}

	}

	@AuxCounters(AuxCounters.Type.OPERATIONS)
	@State(Scope.Thread)
	public static class Outcomes {
		public long reads;
		public long commits;
		public long conflicts;
		public long lockTimeouts;
	}

	private StrolchTransaction open(boolean readOnly) {
		StrolchTransaction tx = this.realm.openTx(this.certificate, getClass(), readOnly);
		tx.silentThreshold(Long.MAX_VALUE, TimeUnit.NANOSECONDS);
		return readOnly ? tx : tx.rollbackOnFailure();
	}

	private StrolchRootElement getRoot(StrolchTransaction tx, Worker worker) {
		return switch (worker.kind) {
			case "Resource" -> tx.getResourceBy(worker.type, worker.id, true);
			case "Order" -> tx.getOrderBy(worker.type, worker.id, true);
			default -> throw new IllegalArgumentException("Unsupported root kind: " + worker.kind);
		};
	}

	private int read(Worker worker) {
		try (StrolchTransaction tx = open(true)) {
			Blackhole.consumeCPU(workTokens);
			return getRoot(tx, worker).getInteger("revision");
		}
	}

	// No dependency on the new exception API: exactly the same harness runs against develop.
	private int update(Worker worker) {
		try {
			try (StrolchTransaction tx = open(false)) {
				Blackhole.consumeCPU(workTokens);
				StrolchRootElement resource = tx.readLock(getRoot(tx, worker));
				resource.setInteger("revision", resource.getInteger("revision") + 1);
				if (resource instanceof Resource r)
					tx.update(r);
				else if (resource instanceof Order o)
					tx.update(o);
				else
					throw new IllegalArgumentException("Unsupported write root");
				tx.commitOnClose();
			}
			worker.successfulUpdates++;
			return 0;
		} catch (RuntimeException e) {
			for (Throwable cause = e; cause != null; cause = cause.getCause()) {
				if (cause.getClass().getName().equals("li.strolch.persistence.api.StrolchTransactionConflictException"))
					return 1;
				if (cause.getClass().getName().equals("li.strolch.utils.concurrent.ElementLockingException"))
					return 2;
			}
			throw e;
		}
	}

	@Benchmark
	public int readOne(Worker worker, Outcomes outcomes) {
		int value = read(worker);
		outcomes.reads++;
		return value;
	}

	@Benchmark
	public void updateIndependent(Worker worker, Outcomes outcomes) {
		switch (update(worker)) {
			case 0 -> outcomes.commits++;
			case 1 -> outcomes.conflicts++;
			case 2 -> outcomes.lockTimeouts++;
			default -> throw new AssertionError();
		}
	}

	@Benchmark
	public void mixedNineReadsOneWrite(Worker worker, Outcomes outcomes, Blackhole blackhole) {
		if (++worker.sequence % 10 == 0)
			updateIndependent(worker, outcomes);
		else
			blackhole.consume(readOne(worker, outcomes));
	}

	@Benchmark
	public long searchType(Outcomes outcomes) {
		try (StrolchTransaction tx = open(true)) {
			long count = new ResourceSearch().types("BenchType0").search(tx).asStream().count();
			outcomes.reads++;
			return count;
		}
	}

	/** Successful logical operation latency, including all retry attempts (never arbitrary failures). */
	@Benchmark
	@BenchmarkMode(Mode.SampleTime)
	@OutputTimeUnit(TimeUnit.MICROSECONDS)
	public int successfulUpdateLatency(Worker worker) {
		for (int attempt = 1; attempt <= 1000; attempt++) {
			int outcome = update(worker);
			if (outcome == 0)
				return attempt;
			if (outcome == 2)
				throw new IllegalStateException("Unexpected lock timeout with disjoint roots");
		}
		throw new IllegalStateException("No successful commit within 1000 attempts");
	}

	@Benchmark
	@BenchmarkMode(Mode.SampleTime)
	@OutputTimeUnit(TimeUnit.MICROSECONDS)
	public int readLatency(Worker worker) {
		return read(worker);
	}
}
