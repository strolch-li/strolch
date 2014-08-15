package ch.eitchnet.communication.chat;

import static ch.eitchnet.communication.chat.ChatMessageVisitor.inboundKey;
import static ch.eitchnet.communication.chat.ChatMessageVisitor.outboundKey;

import java.util.HashMap;
import java.util.Map;
import java.util.Scanner;

import ch.eitchnet.communication.CommandKey;
import ch.eitchnet.communication.CommunicationConnection;
import ch.eitchnet.communication.ConnectionMode;
import ch.eitchnet.communication.ConnectionObserver;
import ch.eitchnet.communication.ConnectionState;
import ch.eitchnet.communication.ConnectionStateObserver;
import ch.eitchnet.communication.IoMessage;
import ch.eitchnet.communication.tcpip.ServerSocketEndpoint;
import ch.eitchnet.communication.tcpip.SocketEndpointConstants;
import ch.eitchnet.communication.tcpip.SocketMessageVisitor;

public class ChatServer implements ConnectionObserver, ConnectionStateObserver {

	private static final String id = "ChatServer"; //$NON-NLS-1$
	private int port;
	private String username;
	private boolean connected;

	public ChatServer(int port, String username) {
		this.port = port;
		this.username = username;
	}

	public void start() {
		ConnectionMode mode = ConnectionMode.ON;

		Map<String, String> parameters = new HashMap<>();
		parameters.put(SocketEndpointConstants.PARAMETER_USE_TIMEOUT, Boolean.FALSE.toString());
		parameters.put(SocketEndpointConstants.PARAMETER_LOCAL_INPUT_PORT, Integer.toString(this.port));
		parameters.put(SocketEndpointConstants.PARAMETER_CLOSE_AFTER_SEND, Boolean.FALSE.toString());
		parameters.put(SocketEndpointConstants.PARAMETER_CLOSE_AFTER_SEND, Boolean.FALSE.toString());

		SocketMessageVisitor messageVisitor = new ChatMessageVisitor(id);
		ServerSocketEndpoint endpoint = new ServerSocketEndpoint();

		CommunicationConnection connection = new CommunicationConnection(id, mode, parameters, endpoint, messageVisitor);
		connection.addConnectionObserver(outboundKey, this);
		connection.addConnectionObserver(inboundKey, this);
		connection.addConnectionStateObserver(this);
		connection.configure();
		connection.start();

		while (!this.connected) {
			synchronized (this) {
				try {
					this.wait(2000l);
				} catch (InterruptedException e) {
					System.err.println("oops: " + e.getMessage()); //$NON-NLS-1$
				}
			}
		}

		System.out.println("Connected. Send messages to user:"); //$NON-NLS-1$
		while (true) {
			@SuppressWarnings("resource")
			Scanner in = new Scanner(System.in);
			//System.out.print(this.username + ": ");
			String line = in.nextLine();
			connection.send(ChatIoMessage.msg(outboundKey, id, this.username, line));
		}
	}

	@Override
	public void notify(CommandKey key, IoMessage message) {
		if (key.equals(inboundKey)) {
			ChatIoMessage chatIoMessage = (ChatIoMessage) message;
			System.out.println(chatIoMessage.getChatMsg());
		}
	}

	@Override
	public void notify(ConnectionState oldState, String oldStateMsg, ConnectionState newState, String newStateMsg) {
		this.connected = newState == ConnectionState.CONNECTED || newState == ConnectionState.IDLE
				|| newState == ConnectionState.WORKING;
		synchronized (this) {
			this.notifyAll();
		}
	}
}
