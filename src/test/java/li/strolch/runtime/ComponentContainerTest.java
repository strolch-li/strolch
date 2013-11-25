package li.strolch.runtime;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.io.File;

import li.strolch.model.Resource;
import li.strolch.runtime.agent.StrolchAgent;
import li.strolch.runtime.component.ComponentContainer;
import li.strolch.runtime.test.PersistenceHandlerTest;
import li.strolch.runtime.test.ServiceHandlerTest;
import li.strolch.runtime.test.ServiceResultTest;

import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

//@Ignore
@SuppressWarnings("nls")
public class ComponentContainerTest {

	private static final Logger logger = LoggerFactory.getLogger(ComponentContainerTest.class);

	@Test
	public void shouldStartEmptyContainer() {

		try {
			File rootPathF = new File("src/test/resources/emptytest");
			startContainer(rootPathF);
		} catch (Exception e) {
			logger.error(e.getMessage(), e);
			throw e;
		}
	}
	@Test
	public void shouldStartTransientContainer() {

		try {
			File rootPathF = new File("src/test/resources/transienttest");
			startContainer(rootPathF);
		} catch (Exception e) {
			logger.error(e.getMessage(), e);
			throw e;
		}
	}


	private void startContainer(File rootPathF) {
		StrolchAgent agent = new StrolchAgent();
		agent.setup(rootPathF);
		agent.initialize();
		agent.start();

		ComponentContainer container = agent.getContainer();

		ServiceHandlerTest serviceHandler = container.getComponent(ServiceHandlerTest.class);
		ServiceResultTest result = serviceHandler.doService();
		assertEquals(1, result.getResult());

		PersistenceHandlerTest persistenceHandler = container.getComponent(PersistenceHandlerTest.class);
		Resource resource = persistenceHandler.getTestResource("@testRes", "Test Res", "Test");
		assertNotNull(resource);
		assertEquals("@testRes", resource.getId());

		container.stop();
		container.destroy();
	}
}
