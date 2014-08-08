package ch.eitchnet.communication;

import static org.junit.Assert.assertEquals;

import java.io.BufferedReader;
import java.io.File;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import ch.eitchnet.communication.file.FileEndpoint;
import ch.eitchnet.communication.file.FileEndpointMode;
import ch.eitchnet.utils.helper.FileHelper;

public class FileEndpointTest extends AbstractEndpointTest {

	public static final String INBOUND_FILENAME = "target/test_in.txt"; //$NON-NLS-1$
	public static final String OUTBOUND_FILENAME = "target/test_out.txt"; //$NON-NLS-1$
	public static final String CONNECTION_ID = "FileTestEndpoint"; //$NON-NLS-1$

	private CommunicationConnection connection;

	@Before
	public void before() {

		new File(OUTBOUND_FILENAME).delete();
		new File(INBOUND_FILENAME).delete();

		Map<String, String> parameters = new HashMap<>();
		parameters.put(FileEndpoint.ENDPOINT_MODE, FileEndpointMode.READ_WRITE.name());
		parameters.put(FileEndpoint.INBOUND_FILENAME, INBOUND_FILENAME);
		parameters.put(FileEndpoint.OUTBOUND_FILENAME, OUTBOUND_FILENAME);

		ConnectionMode mode = ConnectionMode.ON;
		CommunicationEndpoint endpoint = new FileEndpoint();
		StreamMessageVisitor messageVisitor = new StreamMessageVisitorExtension();

		this.connection = new CommunicationConnection(CONNECTION_ID, mode, parameters, endpoint, messageVisitor);
		this.connection.configure();
	}

	@After
	public void after() {
		if (this.connection != null)
			this.connection.stop();
	}

	@Test
	public void testFileEndpoint() throws InterruptedException {

		String inboundFilename = new File(INBOUND_FILENAME).getName();
		String outboundFilename = new File(OUTBOUND_FILENAME).getName();

		// send a message
		this.connection.start();
		TestConnectionObserver outboundObserver = new TestConnectionObserver();
		TestIoMessage message = createTestMessage(outboundFilename, FileEndpointMode.WRITE.name());
		this.connection.addConnectionObserver(message.getKey(), outboundObserver);
		this.connection.send(message);

		// wait till the message has been sent
		waitForMessage(outboundObserver);
		this.connection.stop();
		assertEquals(message.getKey(), outboundObserver.getMessage().getKey());

		// now test reading a file
		this.connection.start();
		CommandKey inboundKey = CommandKey.key(inboundFilename, FileEndpointMode.READ.name());
		TestConnectionObserver inboundObserver = new TestConnectionObserver();
		this.connection.addConnectionObserver(inboundKey, inboundObserver);
		FileHelper.writeStringToFile("Hello\nWorld!", new File(INBOUND_FILENAME)); //$NON-NLS-1$

		// wait for thread to pick up the file
		waitForMessage(inboundObserver);
		assertEquals(inboundKey, inboundObserver.getMessage().getKey());
	}

	public static final class StreamMessageVisitorExtension extends StreamMessageVisitor {
		private String inboundFilename;

		@Override
		public void configure(CommunicationConnection connection) {
			super.configure(connection);
			Map<String, String> parameters = connection.getParameters();
			String filePath = parameters.get(FileEndpoint.INBOUND_FILENAME);
			this.inboundFilename = new File(filePath).getName();
		}

		@Override
		public IoMessage visit(InputStream inputStream) throws Exception {

			BufferedReader reader = new BufferedReader(new InputStreamReader(inputStream));
			List<String> lines = new ArrayList<>();
			String line;
			while ((line = reader.readLine()) != null) {
				lines.add(line);
			}
			return new TestIoMessage(UUID.randomUUID().toString(), CommandKey.key(this.inboundFilename,
					FileEndpointMode.READ.name()), lines);
		}

		@Override
		public void visit(OutputStream outputStream, IoMessage message) throws Exception {
			TestIoMessage msg = (TestIoMessage) message;
			for (String line : msg.getContents()) {
				outputStream.write(line.getBytes());
				outputStream.write('\n');
			}
		}
	}
}
