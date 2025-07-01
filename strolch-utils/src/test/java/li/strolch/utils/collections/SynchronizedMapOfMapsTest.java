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

package li.strolch.utils.collections;

import li.strolch.utils.ThreadHelper;
import org.junit.Before;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicBoolean;

import static li.strolch.utils.collections.SynchronizedCollections.synchronizedMapOfMaps;
import static org.junit.Assert.*;

public class SynchronizedMapOfMapsTest {

	private static final Logger logger = LoggerFactory.getLogger(SynchronizedMapOfMapsTest.class);

	private ExecutorService executorService;

	@Before
	public void before() {
		this.executorService = Executors.newCachedThreadPool();
	}

	@Test
	public void shouldForEach() throws ExecutionException, InterruptedException {

		MapOfMaps<String, String, String> mapOfMaps = buildMapOfMaps();
		AtomicBoolean run = new AtomicBoolean(false);
		Callable<Boolean> addTask = () -> addToMap(mapOfMaps, run);

		Callable<Boolean> iterateTask = () -> {
			while (!run.get()) {
				ThreadHelper.sleep(5L);
			}

			while (run.get()) {
				mapOfMaps.forEach(
						(s, subTypeMap) -> subTypeMap.forEach((s1, s2) -> logger.info("{} {} {}", s, s1, s2)));
				mapOfMaps.getMap("Resource").forEach((s1, s2) -> logger.info("  {} {}", s1, s2));
			}

			return true;
		};

		runTest(addTask, iterateTask, run);
	}

	@Test
	public void shouldIterate() throws ExecutionException, InterruptedException {

		MapOfMaps<String, String, String> mapOfMaps = buildMapOfMaps();
		AtomicBoolean run = new AtomicBoolean(false);
		Callable<Boolean> addTask = () -> addToMap(mapOfMaps, run);
		Callable<Boolean> iterateTask = () -> {
			while (!run.get()) {
				ThreadHelper.sleep(5L);
			}

			while (run.get()) {
				Set<String> types = mapOfMaps.keySet();
				for (String type : types) {
					Map<String, String> subTypeMap = mapOfMaps.getMap(type);
					Set<String> subTypes = subTypeMap.keySet();
					synchronized (mapOfMaps) {
						for (String subType : subTypes) {
							logger.info("{} {} {}", type, subType, mapOfMaps.getElement(type, subType));
						}
					}
				}

				synchronized (mapOfMaps) {
					Map<String, String> resources = mapOfMaps.getMap("Resource");
					for (String value : resources.values()) {
						logger.info("Resource: value: {}", value);
					}
				}
			}

			return true;
		};

		runTest(addTask, iterateTask, run);
	}

	private void runTest(Callable<Boolean> addTask, Callable<Boolean> iterateTask, AtomicBoolean run)
			throws InterruptedException, ExecutionException {

		Future<Boolean> task0 = this.executorService.submit(addTask);
		Future<Boolean> task1 = this.executorService.submit(iterateTask);
		Future<Boolean> task2 = this.executorService.submit(iterateTask);
		Future<Boolean> task3 = this.executorService.submit(iterateTask);
		Future<Boolean> task4 = this.executorService.submit(iterateTask);
		Future<Boolean> task5 = this.executorService.submit(iterateTask);

		run.set(true);
		Thread.sleep(100L);
		run.set(false);

		assertTrue(task0.get());
		assertTrue(task1.get());
		assertTrue(task2.get());
		assertTrue(task3.get());
		assertTrue(task4.get());
		assertTrue(task5.get());
	}

	private Boolean addToMap(MapOfMaps<String, String, String> mapOfMaps, AtomicBoolean run) {
		while (!run.get()) {
			ThreadHelper.sleep(5L);
		}

		while (run.get()) {
			addElement(mapOfMaps, "Resource", "Ball", UUID.randomUUID().toString());
			addElement(mapOfMaps, "Resource", "Car", UUID.randomUUID().toString());
			addElement(mapOfMaps, "Order", "StockOrder", UUID.randomUUID().toString());
			addElement(mapOfMaps, "Order", "ToStock", UUID.randomUUID().toString());
			addElement(mapOfMaps, "Order", "FromStock", UUID.randomUUID().toString());
			addElement(mapOfMaps, "Activity", "FromStock", UUID.randomUUID().toString());
			addElement(mapOfMaps, "Activity", "ToStock", UUID.randomUUID().toString());

			mapOfMaps.removeElement("Resource", "Ball");
			mapOfMaps.removeElement("Order", "ToStock");
			mapOfMaps.removeElement("Activity", "ToStock");
		}

		return true;
	}

	private void addElement(MapOfMaps<String, String, String> mapOfMaps, String type, String subType, String id) {
		//logger.info("Adding " + type + " " + subType + " " + id);
		mapOfMaps.addElement(type, subType, id);
	}

	private MapOfMaps<String, String, String> buildMapOfMaps() {
		MapOfMaps<String, String, String> mapOfMaps = synchronizedMapOfMaps(new MapOfMaps<>(true));
		mapOfMaps.addElement("Resource", "Ball", "yellow");
		mapOfMaps.addElement("Resource", "Car", "car1");
		mapOfMaps.addElement("Order", "StockOrder", "stockOrder1");
		mapOfMaps.addElement("Order", "ToStock", "toStock1");
		mapOfMaps.addElement("Order", "FromStock", "fromStock1");
		mapOfMaps.addElement("Activity", "FromStock", "fromStock1");
		mapOfMaps.addElement("Activity", "ToStock", "toStock1");

		assertEquals("yellow", mapOfMaps.getElement("Resource", "Ball"));
		assertNull(mapOfMaps.getMap("xxx"));
		return mapOfMaps;
	}
}
