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
package ch.eitchnet.communication;

import static org.junit.Assert.assertEquals;

import java.io.BufferedReader;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.InputStreamReader;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import ch.eitchnet.communication.tcpip.ClientSocketEndpoint;
import ch.eitchnet.communication.tcpip.ServerSocketEndpoint;
import ch.eitchnet.communication.tcpip.SocketEndpointConstants;
import ch.eitchnet.communication.tcpip.SocketMessageVisitor;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class SocketEndpointTest extends AbstractEndpointTest {

	private static final String PORT = "45678"; //$NON-NLS-1$
	private static final String HOST = "localhost"; //$NON-NLS-1$
	private static final String CLIENT_CONNECTION_ID = "ClientSocket"; //$NON-NLS-1$
	private static final String SERVER_CONNECTION_ID = "ServerSocket"; //$NON-NLS-1$
	private CommunicationConnection clientConnection;
	private CommunicationConnection serverConnection;

	@Before
	public void before() {

		{
			Map<String, String> parameters = new HashMap<>();
			parameters.put(SocketEndpointConstants.PARAMETER_REMOTE_INPUT_ADDRESS, HOST);
			parameters.put(SocketEndpointConstants.PARAMETER_REMOTE_INPUT_PORT, PORT);

			// we close after send, so that the server can read whole lines, as that is what we are sending
			parameters.put(SocketEndpointConstants.PARAMETER_CLOSE_AFTER_SEND, Boolean.TRUE.toString());

			CommunicationEndpoint endpoint = new ClientSocketEndpoint();
			SocketMessageVisitor messageVisitor = new SocketMessageVisitorExtension();
			this.clientConnection = new CommunicationConnection(CLIENT_CONNECTION_ID, ConnectionMode.ON, parameters,
					endpoint, messageVisitor);
			this.clientConnection.configure();
		}

		{
			Map<String, String> parameters = new HashMap<>();
			parameters.put(SocketEndpointConstants.PARAMETER_LOCAL_INPUT_ADDRESS, HOST);
			parameters.put(SocketEndpointConstants.PARAMETER_LOCAL_INPUT_PORT, PORT);

			CommunicationEndpoint endpoint = new ServerSocketEndpoint();
			SocketMessageVisitor messageVisitor = new SocketMessageVisitorExtension();
			this.serverConnection = new CommunicationConnection(SERVER_CONNECTION_ID, ConnectionMode.ON, parameters,
					endpoint, messageVisitor);
			this.serverConnection.configure();
		}
	}

	@After
	public void after() {
		if (this.clientConnection != null)
			this.clientConnection.stop();
		if (this.serverConnection != null)
			this.serverConnection.stop();
	}

	@Test
	public void testSocketEndpoints() throws Exception {

		this.serverConnection.start();
		Thread.sleep(100);
		this.clientConnection.start();

		TestConnectionObserver serverObserver = new TestConnectionObserver();
		CommandKey inboundKey = CommandKey.key(SERVER_CONNECTION_ID, "lines"); //$NON-NLS-1$
		this.serverConnection.addConnectionObserver(inboundKey, serverObserver);

		TestConnectionObserver clientObserver = new TestConnectionObserver();
		CommandKey outboundKey = CommandKey.key(CLIENT_CONNECTION_ID, "lines"); //$NON-NLS-1$
		this.clientConnection.addConnectionObserver(outboundKey, clientObserver);

		TestIoMessage outboundMsg = createTestMessage(outboundKey, CLIENT_CONNECTION_ID);
		this.clientConnection.send(outboundMsg);
		waitForMessage(clientObserver);
		assertEquals(outboundMsg.getKey(), clientObserver.getMessage().getKey());

		waitForMessage(serverObserver);
		assertEquals(inboundKey, serverObserver.getMessage().getKey());
		assertEquals(outboundMsg.getContents(), ((TestIoMessage) serverObserver.getMessage()).getContents());
	}

	private final class SocketMessageVisitorExtension extends SocketMessageVisitor {
		public SocketMessageVisitorExtension() {
			// do nothing
		}

		@Override
		public void visit(DataInputStream inputStream, DataOutputStream outputStream, IoMessage message)
				throws Exception {
			TestIoMessage msg = (TestIoMessage) message;
			logger.info(MessageFormat
					.format("Writing {0} lines for message {1}", msg.getContents().size(), msg.getId())); //$NON-NLS-1$
			for (String line : msg.getContents()) {
				outputStream.writeBytes(line);
				outputStream.write('\n');
			}
			outputStream.flush();
		}

		@Override
		public IoMessage visit(DataInputStream inputStream, DataOutputStream outputStream) throws Exception {

			List<String> lines = new ArrayList<>();

			// since we are reading whole lines, we must close the stream when we read null i.e. EOF
			try (BufferedReader reader = new BufferedReader(new InputStreamReader(inputStream))) {
				String line;
				logger.info("Reading from stream..."); //$NON-NLS-1$
				while ((line = reader.readLine()) != null) {
					lines.add(line);
				}
			}
			logger.info(MessageFormat.format("Read {0} lines from stream.", lines.size())); //$NON-NLS-1$

			return new TestIoMessage(UUID.randomUUID().toString(),
					CommandKey.key(SERVER_CONNECTION_ID, "lines"), SERVER_CONNECTION_ID, lines); //$NON-NLS-1$
		}
	}
}
