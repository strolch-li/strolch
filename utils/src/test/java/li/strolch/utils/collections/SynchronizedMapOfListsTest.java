package li.strolch.utils.collections;

import static li.strolch.utils.collections.SynchronizedCollections.synchronizedMapOfLists;
import static org.junit.Assert.*;

import java.util.Arrays;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicBoolean;

import org.junit.Before;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class SynchronizedMapOfListsTest {

	private static final Logger logger = LoggerFactory.getLogger(SynchronizedMapOfListsTest.class);

	private ExecutorService executorService;

	@Before
	public void before() {
		this.executorService = Executors.newCachedThreadPool();
	}

	@Test
	public void shouldForEach() throws ExecutionException, InterruptedException {

		MapOfLists<String, String> mapOfLists = buildMapOfLists();
		AtomicBoolean run = new AtomicBoolean(false);
		Callable<Boolean> addTask = () -> addToMap(mapOfLists, run);

		Callable<Boolean> iterateTask = () -> {
			for (; ; ) {
				if (run.get())
					break;
			}

			while (run.get()) {
				mapOfLists.forEach((s, list) -> list.forEach(s1 -> logger.info("{} {}", s, s1)));
				mapOfLists.getList("Resource").forEach(s1 -> logger.info("  {}", s1));
			}

			return true;
		};

		runTest(addTask, iterateTask, run);
	}

	@Test
	public void shouldIterate() throws ExecutionException, InterruptedException {

		MapOfLists<String, String> mapOfLists = buildMapOfLists();
		AtomicBoolean run = new AtomicBoolean(false);
		Callable<Boolean> addTask = () -> addToMap(mapOfLists, run);
		Callable<Boolean> iterateTask = () -> {
			for (; ; ) {
				if (run.get())
					break;
			}

			while (run.get()) {
				Set<String> types = mapOfLists.keySet();
				for (String type : types) {
					synchronized (mapOfLists) {
						for (String id : mapOfLists.getList(type)) {
							logger.info("{} {}", type, id);
						}
					}
				}

				synchronized (mapOfLists) {
					List<String> resources = mapOfLists.getList("Resource");
					for (String value : resources) {
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

	private Boolean addToMap(MapOfLists<String, String> mapOfLists, AtomicBoolean run) {
		for (; ; ) {
			if (run.get())
				break;
		}

		while (run.get()) {
			addElement(mapOfLists, "Resource", UUID.randomUUID().toString());
			addElement(mapOfLists, "Resource", UUID.randomUUID().toString());
			addElement(mapOfLists, "Order", UUID.randomUUID().toString());
			addElement(mapOfLists, "Order", UUID.randomUUID().toString());
			addElement(mapOfLists, "Order", UUID.randomUUID().toString());
			addElement(mapOfLists, "Activity", UUID.randomUUID().toString());
			addElement(mapOfLists, "Activity", UUID.randomUUID().toString());

			mapOfLists.removeElement("Resource", "Ball");
			mapOfLists.removeElement("Order", "ToStock");
			mapOfLists.removeElement("Activity", "ToStock");
		}

		return true;
	}

	private void addElement(MapOfLists<String, String> mapOfLists, String type, String id) {
		//logger.info("Adding " + type + " " + id);
		mapOfLists.addElement(type, id);
	}

	private MapOfLists<String, String> buildMapOfLists() {
		MapOfLists<String, String> mapOfLists = synchronizedMapOfLists(new MapOfLists<>(true));
		mapOfLists.addElement("Resource", "Ball");
		mapOfLists.addElement("Resource", "Car");
		mapOfLists.addElement("Order", "StockOrder");
		mapOfLists.addElement("Order", "ToStock");
		mapOfLists.addElement("Order", "FromStock");
		mapOfLists.addElement("Activity", "FromStock");
		mapOfLists.addElement("Activity", "ToStock");

		assertEquals(Arrays.asList("Ball", "Car"), mapOfLists.getList("Resource"));
		assertNull(mapOfLists.getList("xxx"));
		return mapOfLists;
	}
}
