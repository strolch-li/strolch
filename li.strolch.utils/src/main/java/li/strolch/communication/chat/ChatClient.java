package li.strolch.communication.chat;

import static li.strolch.communication.chat.ChatMessageVisitor.inboundKey;
import static li.strolch.communication.chat.ChatMessageVisitor.outboundKey;

import java.net.InetAddress;
import java.util.HashMap;
import java.util.Map;
import java.util.Scanner;

import li.strolch.communication.CommandKey;
import li.strolch.communication.CommunicationConnection;
import li.strolch.communication.ConnectionMode;
import li.strolch.communication.ConnectionObserver;
import li.strolch.communication.ConnectionState;
import li.strolch.communication.ConnectionStateObserver;
import li.strolch.communication.IoMessage;
import li.strolch.communication.tcpip.ClientSocketEndpoint;
import li.strolch.communication.tcpip.SocketEndpointConstants;
import li.strolch.communication.tcpip.SocketMessageVisitor;

public class ChatClient implements ConnectionObserver, ConnectionStateObserver {

	private static final String id = "ChatClient"; //$NON-NLS-1$
	private InetAddress host;
	private int port;
	private String username;
	private boolean connected;

	public ChatClient(InetAddress host, int port, String username) {
		this.host = host;
		this.port = port;
		this.username = username;
	}

	public void start() {
		ConnectionMode mode = ConnectionMode.ON;

		Map<String, String> parameters = new HashMap<>();
		parameters.put(SocketEndpointConstants.PARAMETER_RETRY, "10000"); //$NON-NLS-1$
		parameters.put(SocketEndpointConstants.PARAMETER_USE_TIMEOUT, Boolean.FALSE.toString());
		parameters.put(SocketEndpointConstants.PARAMETER_REMOTE_INPUT_ADDRESS, this.host.getHostAddress());
		parameters.put(SocketEndpointConstants.PARAMETER_REMOTE_INPUT_PORT, Integer.toString(this.port));
		parameters.put(SocketEndpointConstants.PARAMETER_CLOSE_AFTER_SEND, Boolean.FALSE.toString());

		SocketMessageVisitor messageVisitor = new ChatMessageVisitor(id);
		ClientSocketEndpoint endpoint = new ClientSocketEndpoint();

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
			notifyAll();
		}
	}
}
