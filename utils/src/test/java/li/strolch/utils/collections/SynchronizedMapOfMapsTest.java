package li.strolch.utils.collections;

import static li.strolch.utils.collections.SynchronizedCollections.synchronizedMapOfMaps;
import static org.junit.Assert.*;

import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicBoolean;

import org.junit.Before;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

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
			for (; ; ) {
				if (run.get())
					break;
			}

			while (run.get()) {
				mapOfMaps.forEach(
						(s, subTypeMap) -> subTypeMap.forEach((s1, s2) -> logger.info(s + " " + s1 + " " + s2)));
				mapOfMaps.getMap("Resource").forEach((s1, s2) -> logger.info("  " + s1 + " " + s2));
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
			for (; ; ) {
				if (run.get())
					break;
			}

			while (run.get()) {
				Set<String> types = mapOfMaps.keySet();
				for (String type : types) {
					Map<String, String> subTypeMap = mapOfMaps.getMap(type);
					Set<String> subTypes = subTypeMap.keySet();
					synchronized (mapOfMaps) {
						for (String subType : subTypes) {
							logger.info(type + " " + subType + " " + mapOfMaps.getElement(type, subType));
						}
					}
				}

				synchronized (mapOfMaps) {
					Map<String, String> resources = mapOfMaps.getMap("Resource");
					for (String value : resources.values()) {
						logger.info("Resource: value: " + value);
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
		Thread.sleep(20L);
		run.set(false);

		Boolean result0 = task0.get();
		Boolean result1 = task1.get();
		Boolean result2 = task2.get();
		Boolean result3 = task3.get();
		Boolean result4 = task4.get();
		Boolean result5 = task5.get();

		assertTrue(result0);
		assertTrue(result1);
		assertTrue(result2);
		assertTrue(result3);
		assertTrue(result4);
		assertTrue(result5);
	}

	private Boolean addToMap(MapOfMaps<String, String, String> mapOfMaps, AtomicBoolean run) {
		for (; ; ) {
			if (run.get())
				break;
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
