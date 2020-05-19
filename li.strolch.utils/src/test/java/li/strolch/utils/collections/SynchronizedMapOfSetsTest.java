package li.strolch.utils.collections;

import static li.strolch.utils.collections.SynchronizedCollections.synchronizedMapOfSets;
import static org.junit.Assert.*;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicBoolean;

import org.junit.Before;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class SynchronizedMapOfSetsTest {

	private static final Logger logger = LoggerFactory.getLogger(SynchronizedMapOfSetsTest.class);

	private ExecutorService executorService;

	@Before
	public void before() {
		this.executorService = Executors.newCachedThreadPool();
	}

	@Test
	public void shouldForEach() throws ExecutionException, InterruptedException {

		MapOfSets<String, String> mapOfSets = buildMapOfSets();
		AtomicBoolean run = new AtomicBoolean(false);
		Callable<Boolean> addTask = () -> addToMap(mapOfSets, run);

		Callable<Boolean> iterateTask = () -> {
			for (; ; ) {
				if (run.get())
					break;
			}

			while (run.get()) {
				mapOfSets.forEach((s, list) -> list.forEach(s1 -> logger.info(s + " " + s1)));
				mapOfSets.getSet("Resource").forEach(s1 -> logger.info("  " + s1));
			}

			return true;
		};

		runTest(addTask, iterateTask, run);
	}

	@Test
	public void shouldIterate() throws ExecutionException, InterruptedException {

		MapOfSets<String, String> mapOfSets = buildMapOfSets();
		AtomicBoolean run = new AtomicBoolean(false);
		Callable<Boolean> addTask = () -> addToMap(mapOfSets, run);
		Callable<Boolean> iterateTask = () -> {
			for (; ; ) {
				if (run.get())
					break;
			}

			while (run.get()) {
				Set<String> types = mapOfSets.keySet();
				for (String type : types) {
					synchronized (mapOfSets) {
						for (String id : mapOfSets.getSet(type)) {
							logger.info(type + " " + id);
						}
					}
				}

				synchronized (mapOfSets) {
					Set<String> resources = mapOfSets.getSet("Resource");
					for (String value : resources) {
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
		Thread.sleep(1000L);
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

	private Boolean addToMap(MapOfSets<String, String> mapOfSets, AtomicBoolean run) {
		for (; ; ) {
			if (run.get())
				break;
		}

		while (run.get()) {
			addElement(mapOfSets, "Resource", UUID.randomUUID().toString());
			addElement(mapOfSets, "Resource", UUID.randomUUID().toString());
			addElement(mapOfSets, "Order", UUID.randomUUID().toString());
			addElement(mapOfSets, "Order", UUID.randomUUID().toString());
			addElement(mapOfSets, "Order", UUID.randomUUID().toString());
			addElement(mapOfSets, "Activity", UUID.randomUUID().toString());
			addElement(mapOfSets, "Activity", UUID.randomUUID().toString());

			mapOfSets.removeElement("Resource", "Ball");
			mapOfSets.removeElement("Order", "ToStock");
			mapOfSets.removeElement("Activity", "ToStock");
		}

		return true;
	}

	private void addElement(MapOfSets<String, String> mapOfSets, String type, String id) {
		logger.info("Adding " + type + " " + id);
		mapOfSets.addElement(type, id);
	}

	private MapOfSets<String, String> buildMapOfSets() {
		MapOfSets<String, String> mapOfSets = synchronizedMapOfSets(new MapOfSets<>(true));
		mapOfSets.addElement("Resource", "Ball");
		mapOfSets.addElement("Resource", "Car");
		mapOfSets.addElement("Order", "StockOrder");
		mapOfSets.addElement("Order", "ToStock");
		mapOfSets.addElement("Order", "FromStock");
		mapOfSets.addElement("Activity", "FromStock");
		mapOfSets.addElement("Activity", "ToStock");

		assertEquals(new HashSet<>(Arrays.asList("Ball", "Car")), mapOfSets.getSet("Resource"));
		assertNull(mapOfSets.getSet("xxx"));
		return mapOfSets;
	}
}
