package ch.eitchnet.communication;

import static org.junit.Assert.assertEquals;

import java.util.HashMap;
import java.util.Map;

import org.junit.Before;
import org.junit.Test;
import org.slf4j.Logger;

import ch.eitchnet.communication.console.ConsoleEndpoint;
import ch.eitchnet.communication.console.ConsoleMessageVisitor;

public class ConsoleEndpointTest extends AbstractEndpointTest {

	private static final String CONNECTION_ID = "Console"; //$NON-NLS-1$
	private CommunicationConnection connection;

	@Before
	public void before() {

		Map<String, String> parameters = new HashMap<>();
		CommunicationEndpoint endpoint = new ConsoleEndpoint();
		ConsoleMessageVisitor messageVisitor = new ConsoleMessageVisitorExtension();
		this.connection = new CommunicationConnection(CONNECTION_ID, ConnectionMode.ON, parameters, endpoint,
				messageVisitor);
		this.connection.configure();
	}

	@Test
	public void testConsoleEndpoint() throws InterruptedException {

		this.connection.start();
		
		CommandKey key = CommandKey.key(CONNECTION_ID, "logger"); //$NON-NLS-1$
		TestIoMessage msg = createTestMessage(key);

		TestConnectionObserver observer = new TestConnectionObserver();
		this.connection.addConnectionObserver(key, observer);
		this.connection.send(msg);
		waitForMessage(observer);

		assertEquals(msg.getKey(), observer.getMessage().getKey());

	}

	private final class ConsoleMessageVisitorExtension extends ConsoleMessageVisitor {
		public ConsoleMessageVisitorExtension() {
			// no-op
		}

		@Override
		public void visit(Logger logger, IoMessage message) throws Exception {
			TestIoMessage msg = (TestIoMessage) message;
			for (String line : msg.getContents()) {
				logger.info(line);
			}
		}
	}
}
