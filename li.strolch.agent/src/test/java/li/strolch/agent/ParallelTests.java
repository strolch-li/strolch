package li.strolch.agent;

import static java.util.stream.Collectors.toList;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.ForkJoinTask;

import li.strolch.RuntimeMock;
import li.strolch.agent.api.StrolchAgent;
import li.strolch.agent.api.StrolchRealm;
import li.strolch.model.ModelGenerator;
import li.strolch.model.StrolchElement;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Certificate;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ParallelTests {

	public static final Logger logger = LoggerFactory.getLogger(ParallelTests.class);

	private static final String TARGET_PATH = "target/" + ParallelTests.class.getSimpleName();
	private static final String SOURCE_PATH = "src/test/resources/transienttest";

	private static RuntimeMock runtimeMock;
	private static Certificate cert;

	@BeforeClass
	public static void beforeClass() {
		runtimeMock = new RuntimeMock(TARGET_PATH, SOURCE_PATH).mockRuntime();
		runtimeMock.startContainer();
		cert = runtimeMock.getPrivilegeHandler().authenticate("test", "test".toCharArray());

		// generate a some elements
		StrolchRealm realm = runtimeMock.getAgent().getContainer().getRealm(cert);
		try (StrolchTransaction tx = realm.openTx(cert, ParallelTests.class)) {

			for (int i = 0; i < 5000; i++) {
				String id = StrolchAgent.getUniqueId();
				tx.add(ModelGenerator.createResource(id, id, "SomeType"));
			}

			tx.commitOnClose();
		}
	}

	@AfterClass
	public static void afterClass() {
		if (cert != null)
			runtimeMock.getPrivilegeHandler().invalidate(cert);
		if (runtimeMock != null)
			runtimeMock.destroyRuntime();
	}

	@Test
	public void shouldAccessElementMapsInParallel() {

		ForkJoinPool forkJoinPool = ForkJoinPool.commonPool();

		List<ParallelTask> tasks = new ArrayList<>();
		for (int i = 0; i < 5; i++) {
			ParallelTask task = new ParallelTask();
			tasks.add(task);
			forkJoinPool.execute(task);
		}

		for (ParallelTask task : tasks) {
			task.join();
		}
	}

	public class ParallelTask extends ForkJoinTask<Void> {

		@Override
		public Void getRawResult() {
			return null;
		}

		@Override
		protected void setRawResult(Void value) {
			//
		}

		@Override
		protected boolean exec() {

			long start = System.currentTimeMillis();

			while (System.currentTimeMillis() - start < 1000L) {

				StrolchRealm realm = runtimeMock.getAgent().getContainer().getRealm(cert);
				try (StrolchTransaction tx = realm.openTx(cert, ParallelTests.class)) {

					List<String> ids = tx.streamResources().map(StrolchElement::getId).collect(toList());
					logger.info("There are " + ids.size() + " Resources!");

					ids = tx.streamResources("SomeType", "TestType").map(StrolchElement::getId).collect(toList());
					logger.info("There are " + ids.size() + " Resources of type SomeType");
				}
			}

			return true;
		}
	}
}
