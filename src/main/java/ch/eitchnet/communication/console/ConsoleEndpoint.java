package ch.eitchnet.communication.console;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ch.eitchnet.communication.CommunicationConnection;
import ch.eitchnet.communication.CommunicationEndpoint;
import ch.eitchnet.communication.ConnectionMessages;
import ch.eitchnet.communication.ConnectionState;
import ch.eitchnet.communication.IoMessage;
import ch.eitchnet.communication.IoMessageVisitor;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class ConsoleEndpoint implements CommunicationEndpoint {

	private static final Logger logger = LoggerFactory.getLogger(ConsoleEndpoint.class);
	private CommunicationConnection connection;
	private ConsoleMessageVisitor messageVisitor;

	@Override
	public void configure(CommunicationConnection connection, IoMessageVisitor messageVisitor) {
		this.connection = connection;
		ConnectionMessages.assertLegalMessageVisitor(this.getClass(), ConsoleMessageVisitor.class, messageVisitor);
		this.messageVisitor = (ConsoleMessageVisitor) messageVisitor;
	}

	@Override
	public void start() {
		this.connection.notifyStateChange(ConnectionState.IDLE, ConnectionState.IDLE.toString());
	}

	@Override
	public void stop() {
		this.connection.notifyStateChange(ConnectionState.DISCONNECTED, ConnectionState.DISCONNECTED.toString());
	}

	@Override
	public void reset() {
		this.connection.notifyStateChange(ConnectionState.INITIALIZED, ConnectionState.INITIALIZED.toString());
	}

	@Override
	public String getLocalUri() {
		return "console"; //$NON-NLS-1$
	}

	@Override
	public String getRemoteUri() {
		return "console"; //$NON-NLS-1$
	}

	@Override
	public void send(IoMessage message) throws Exception {
		this.connection.notifyStateChange(ConnectionState.WORKING, ConnectionState.WORKING.toString());
		try {
			this.messageVisitor.visit(logger, message);
		} finally {
			this.connection.notifyStateChange(ConnectionState.IDLE, ConnectionState.IDLE.toString());
		}
	}
}
