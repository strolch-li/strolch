package li.strolch.agent;

import static org.junit.Assert.assertEquals;

import java.security.SecureRandom;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

import li.strolch.RuntimeMock;
import li.strolch.agent.api.Observer;
import li.strolch.agent.api.StrolchRealm;
import li.strolch.model.Resource;
import li.strolch.model.StrolchRootElement;
import li.strolch.model.Tags;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Certificate;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ObserverHandlerTests {

	public static final Logger logger = LoggerFactory.getLogger(ObserverHandlerTests.class);

	private static final String TARGET_PATH = "target/" + ObserverHandlerTests.class.getSimpleName();
	private static final String SOURCE_PATH = "src/test/resources/transienttest";
	public static final int MAX_WAIT_PER_UPDATE = 100;
	public static final int UPDATE_SIZE = 50;

	private static RuntimeMock runtimeMock;
	private static Certificate cert;

	private static final AtomicInteger updateCount = new AtomicInteger(0);

	@BeforeClass
	public static void beforeClass() {
		runtimeMock = new RuntimeMock(TARGET_PATH, SOURCE_PATH).mockRuntime();
		runtimeMock.startContainer();
		cert = runtimeMock.getPrivilegeHandler().authenticate("test", "test".toCharArray());

		// register an observer
		StrolchRealm realm = runtimeMock.getAgent().getContainer().getRealm(cert);
		realm.getObserverHandler().registerObserver(Tags.RESOURCE, new TestObserver());
	}

	@AfterClass
	public static void afterClass() {
		if (cert != null)
			runtimeMock.getPrivilegeHandler().invalidate(cert);
		if (runtimeMock != null)
			runtimeMock.destroyRuntime();
	}

	@Test
	public void shouldNotifyObservers() throws InterruptedException {

		updateCount.set(0);
		long expectedCount = 0;

		for (int i = 0; i < UPDATE_SIZE; i++) {
			StrolchRealm realm = runtimeMock.getAgent().getContainer().getRealm(cert);
			try (StrolchTransaction tx = realm.openTx(cert, ParallelTests.class, false)) {
				tx.add(new Resource("res_" + i, "Resource " + i, "MyType"));
				tx.commitOnClose();
			}

			expectedCount++;
		}

		long start = System.currentTimeMillis();
		long maxWaitTime = MAX_WAIT_PER_UPDATE * expectedCount;
		while (updateCount.get() != expectedCount) {
			Thread.sleep(50L);
			if ((System.currentTimeMillis() - start) > maxWaitTime)
				throw new IllegalStateException("Updates didn't complete in " + maxWaitTime + "ms");
		}

		assertEquals("Expected " + UPDATE_SIZE + " updates!", UPDATE_SIZE, updateCount.get());
	}

	static class TestObserver implements Observer {
		private final SecureRandom random = new SecureRandom();

		@Override
		public void add(String key, List<StrolchRootElement> elements) throws Exception {
			int wait = random.nextInt(MAX_WAIT_PER_UPDATE);
			Thread.sleep(wait);
			for (StrolchRootElement element : elements) {
				logger.info("Added " + key + " " + element.getLocator() + " took " + wait + "ms");
			}

			updateCount.incrementAndGet();
		}
	}
}
