/*
 * Copyright 2014 Robert von Burg <eitch@eitchnet.ch>
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
package li.strolch.communication;

import static org.junit.Assert.assertEquals;

import java.util.HashMap;
import java.util.Map;

import org.junit.Before;
import org.junit.Test;
import org.slf4j.Logger;

import li.strolch.communication.CommandKey;
import li.strolch.communication.CommunicationConnection;
import li.strolch.communication.CommunicationEndpoint;
import li.strolch.communication.ConnectionMode;
import li.strolch.communication.IoMessage;
import li.strolch.communication.console.ConsoleEndpoint;
import li.strolch.communication.console.ConsoleMessageVisitor;

/**
 * @author Robert von Burg &lt;eitch@eitchnet.ch&gt;
 */
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
		TestIoMessage msg = createTestMessage(key, CONNECTION_ID);

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
