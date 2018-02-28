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
package li.strolch.communication.tcpip;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.net.InetAddress;
import java.net.Socket;
import java.net.UnknownHostException;
import java.text.MessageFormat;
import java.util.Map;

import li.strolch.utils.helper.ExceptionHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import li.strolch.communication.CommunicationConnection;
import li.strolch.communication.CommunicationEndpoint;
import li.strolch.communication.ConnectionException;
import li.strolch.communication.ConnectionMessages;
import li.strolch.communication.ConnectionState;
import li.strolch.communication.IoMessage;
import li.strolch.communication.IoMessageVisitor;
import li.strolch.communication.IoMessage.State;
import li.strolch.utils.helper.StringHelper;

/**
 * <p>
 * This {@link CommunicationEndpoint} is an abstract implementation with everything needed to connect through a
 * {@link Socket} to a remote server which is listening for incoming {@link Socket} connections. This {@link Socket}
 * endpoint can send messages to the remote side, as well as receive messages from the remote side
 * </p>
 * <p>
 * This endpoint is maintained as a client connection. This means that this endpoint opens the {@link Socket} to the
 * remote server
 * </p>
 *
 * @author Robert von Burg &lt;eitch@eitchnet.ch&gt;
 */
public class ClientSocketEndpoint implements CommunicationEndpoint {

	protected static final Logger logger = LoggerFactory.getLogger(ClientSocketEndpoint.class);

	// state variables
	private boolean connected;
	private boolean closed;
	private long lastConnect;
	private boolean useTimeout;
	private int timeout;
	private long retry;
	private boolean clearOnConnect;
	private boolean connectOnStart;
	private boolean closeAfterSend;

	// remote address
	private String remoteInputAddressS;
	private int remoteInputPort;
	private String localOutputAddressS;
	private int localOutputPort;

	private InetAddress remoteInputAddress;
	private InetAddress localOutputAddress;

	// connection
	private Socket socket;

	protected DataOutputStream outputStream;
	protected DataInputStream inputStream;

	protected CommunicationConnection connection;

	protected SocketMessageVisitor messageVisitor;

	/**
	 * Default constructor
	 */
	public ClientSocketEndpoint() {
		this.connected = false;
		this.closed = true;
	}

	/**
	 * Checks the state of the connection and returns true if {@link Socket} is connected and ready for transmission,
	 * false otherwise
	 *
	 * @return true if {@link Socket} is connected and ready for transmission, false otherwise
	 */
	protected boolean checkConnection() {
		return !this.closed && this.connected && (this.socket != null && !this.socket.isClosed() && this.socket
				.isBound() && this.socket.isConnected() && !this.socket.isInputShutdown() && !this.socket
				.isOutputShutdown());
	}

	/**
	 * Establishes a {@link Socket} connection to the remote server. This method blocks till a connection is
	 * established. In the event of a connection failure, the method waits a configured time before retrying
	 */
	protected void openConnection() {

		ConnectionState state = this.connection.getState();

		// do not allow connecting if state is
		// - CREATED
		// - CONNECTING
		// - WAITING
		// - CLOSED
		if (state == ConnectionState.CREATED || state == ConnectionState.CONNECTING || state == ConnectionState.WAITING
				|| state == ConnectionState.DISCONNECTED) {

			ConnectionMessages.throwIllegalConnectionState(state, ConnectionState.CONNECTING);
		}

		// first close the connection
		closeConnection();

		while (!this.connected && !this.closed) {
			try {

				this.connection.notifyStateChange(ConnectionState.CONNECTING, ConnectionState.CONNECTING.toString());

				// only try in proper intervals
				long currentTime = System.currentTimeMillis();
				long timeDifference = currentTime - this.lastConnect;
				if (timeDifference < this.retry) {
					long wait = (this.retry - timeDifference);
					logger.info(MessageFormat.format("Waiting: {0}ms", wait)); //$NON-NLS-1$

					this.connection.notifyStateChange(ConnectionState.WAITING, ConnectionState.WAITING.toString());
					Thread.sleep(wait);
					this.connection
							.notifyStateChange(ConnectionState.CONNECTING, ConnectionState.CONNECTING.toString());
				}

				// don't try and connect if we are closed!
				if (this.closed) {
					logger.error("The connection has been closed and can not be connected"); //$NON-NLS-1$
					closeConnection();
					this.connection.notifyStateChange(ConnectionState.DISCONNECTED, null);
					return;
				}

				// keep track of the time of this connection attempt
				this.lastConnect = System.currentTimeMillis();

				// open the socket
				if (this.localOutputAddress != null) {
					String msg = "Opening connection to {0}:{1} from {2}:{3}..."; //$NON-NLS-1$
					logger.info(MessageFormat.format(msg, this.remoteInputAddress.getHostAddress(),
							Integer.toString(this.remoteInputPort), this.localOutputAddress.getHostAddress(),
							Integer.toString(this.localOutputPort)));
					this.socket = new Socket(this.remoteInputAddress, this.remoteInputPort, this.localOutputAddress,
							this.localOutputPort);
				} else {
					String msg = "Opening connection to {0}:{1}..."; //$NON-NLS-1$
					logger.info(MessageFormat.format(msg, this.remoteInputAddress.getHostAddress(),
							Integer.toString(this.remoteInputPort)));
					this.socket = new Socket(this.remoteInputAddress, this.remoteInputPort);
				}

				// configure the socket
				if (logger.isDebugEnabled()) {
					String msg = "BufferSize (send/read): {0} / {1} SoLinger: {2} TcpNoDelay: {3}"; //$NON-NLS-1$
					logger.info(MessageFormat
							.format(msg, this.socket.getSendBufferSize(), this.socket.getReceiveBufferSize(),
									this.socket.getSoLinger(), this.socket.getTcpNoDelay()));
				}
				//outputSocket.setSendBufferSize(1);
				//outputSocket.setSoLinger(true, 0);
				//outputSocket.setTcpNoDelay(true);

				// activate connection timeout
				if (this.useTimeout) {
					this.socket.setSoTimeout(this.timeout);
				}

				// get the streams
				this.outputStream = new DataOutputStream(this.socket.getOutputStream());
				this.inputStream = new DataInputStream(this.socket.getInputStream());

				if (this.clearOnConnect) {
					// clear the input stream
					int available = this.inputStream.available();
					logger.info(MessageFormat.format("clearOnConnect: skipping {0} bytes.", available)); //$NON-NLS-1$
					this.inputStream.skip(available);
				}

				String msg = "Connected {0}: {1}:{2} with local side {3}:{4}"; //$NON-NLS-1$
				logger.info(MessageFormat.format(msg, this.connection.getId(), this.remoteInputAddressS,
						Integer.toString(this.remoteInputPort), this.socket.getLocalAddress().getHostAddress(),
						Integer.toString(this.socket.getLocalPort())));

				// we are connected!
				this.connection.notifyStateChange(ConnectionState.CONNECTED, ConnectionState.CONNECTED.toString());
				this.connected = true;

			} catch (InterruptedException e) {
				logger.info("Interrupted!"); //$NON-NLS-1$
				this.closed = true;
				this.connection.notifyStateChange(ConnectionState.DISCONNECTED, null);
			} catch (Exception e) {
				String msg = "Error while connecting to {0}:{1}: {2}"; //$NON-NLS-1$
				logger.error(MessageFormat.format(msg, this.remoteInputAddressS, Integer.toString(this.remoteInputPort),
						ExceptionHelper.formatExceptionMessage(e)));
				this.connection.notifyStateChange(ConnectionState.BROKEN, e.getLocalizedMessage());
			}
		}
	}

	/**
	 * closes the connection HARD by calling close() on the streams and socket. All Exceptions are caught to make sure
	 * that the connections are cleaned up
	 */
	protected void closeConnection() {

		this.connected = false;
		this.connection.notifyStateChange(ConnectionState.BROKEN, null);

		if (this.outputStream != null) {
			try {
				this.outputStream.close();
			} catch (IOException e) {
				logger.error(
						MessageFormat.format("Error closing OutputStream: {0}", e.getLocalizedMessage())); //$NON-NLS-1$
			} finally {
				this.outputStream = null;
			}
		}

		if (this.inputStream != null) {
			try {
				this.inputStream.close();
			} catch (IOException e) {
				logger.error(
						MessageFormat.format("Error closing InputStream: {0}", e.getLocalizedMessage())); //$NON-NLS-1$
			} finally {
				this.inputStream = null;
			}
		}

		if (this.socket != null) {
			try {
				this.socket.close();
			} catch (IOException e) {
				logger.error(
						MessageFormat.format("Error closing OutputSocket: {0}", e.getLocalizedMessage())); //$NON-NLS-1$
			} finally {
				this.socket = null;
			}

			String msg = "Socket closed for connection {0} at remote input address {1}:{2}"; //$NON-NLS-1$
			logger.info(MessageFormat.format(msg, this.connection.getId(), this.remoteInputAddressS,
					Integer.toString(this.remoteInputPort)));
		}
	}

	/**
	 * <p>
	 * Configures this {@link ClientSocketEndpoint}
	 * </p>
	 * gets the parameter map from the connection and reads the following parameters from the map:
	 * <ul>
	 * <li>remoteInputAddress - the IP or Hostname of the remote server</li>
	 * <li>remoteInputPort - the port to which the socket should be established</li>
	 * <li>localOutputAddress - the IP or Hostname of the local server (if null, then the network layer will decide)</li>
	 * <li>localOutputPort - the local port from which the socket should go out of (if null, then the network layer will
	 * decide)</li>
	 * <li>retry - a configured retry wait time. Default is {@link SocketEndpointConstants#RETRY}</li>
	 * <li>timeout - the timeout after which an idle socket is deemed dead. Default is
	 * {@link SocketEndpointConstants#TIMEOUT}</li>
	 * <li>useTimeout - if true, then the timeout is activated, otherwise it is. default is
	 * {@link SocketEndpointConstants#USE_TIMEOUT}</li>
	 * <li>clearOnConnect - if true, then the after a successful connect the input is cleared by discarding all
	 * available bytes. This can be useful in cases where the channel is clogged with stale data. default is
	 * {@link SocketEndpointConstants#CLEAR_ON_CONNECT}</li>
	 * <li>connectOnStart - if true, then when the connection is started, the connection to the remote address is
	 * attempted. default is {@link SocketEndpointConstants#CONNECT_ON_START}
	 * </ul>
	 *
	 * @see CommunicationEndpoint#configure(CommunicationConnection, IoMessageVisitor)
	 */
	@Override
	public void configure(CommunicationConnection connection, IoMessageVisitor messageVisitor) {
		if (this.connection != null && connection.getState().compareTo(ConnectionState.INITIALIZED) > 0) {
			String msg = "{0}:{1} already configured."; //$NON-NLS-1$
			logger.warn(MessageFormat.format(msg, this.getClass().getSimpleName(), connection.getId()));
			return;
		}

		ConnectionMessages.assertLegalMessageVisitor(this.getClass(), SocketMessageVisitor.class, messageVisitor);
		this.messageVisitor = (SocketMessageVisitor) messageVisitor;
		this.connection = connection;
		configure();
	}

	private void configure() {
		Map<String, String> parameters = this.connection.getParameters();

		this.remoteInputAddressS = parameters.get(SocketEndpointConstants.PARAMETER_REMOTE_INPUT_ADDRESS);
		String remoteInputPortS = parameters.get(SocketEndpointConstants.PARAMETER_REMOTE_INPUT_PORT);
		this.localOutputAddressS = parameters.get(SocketEndpointConstants.PARAMETER_LOCAL_OUTPUT_ADDRESS);
		String localOutputPortS = parameters.get(SocketEndpointConstants.PARAMETER_LOCAL_OUTPUT_PORT);

		// parse remote input Address to InetAddress object
		try {
			this.remoteInputAddress = InetAddress.getByName(this.remoteInputAddressS);
		} catch (UnknownHostException e) {
			throw ConnectionMessages.throwInvalidParameter(ClientSocketEndpoint.class,
					SocketEndpointConstants.PARAMETER_REMOTE_INPUT_ADDRESS, this.remoteInputAddressS);
		}

		// parse remote input address port to integer
		try {
			this.remoteInputPort = Integer.parseInt(remoteInputPortS);
		} catch (NumberFormatException e) {
			throw ConnectionMessages.throwInvalidParameter(ClientSocketEndpoint.class,
					SocketEndpointConstants.PARAMETER_REMOTE_INPUT_PORT, remoteInputPortS);
		}

		// if local output address is not set, then we will use the localhost InetAddress
		if (this.localOutputAddressS == null || this.localOutputAddressS.length() == 0) {
			logger.debug("No localOutputAddress set. Using localhost"); //$NON-NLS-1$
		} else {

			// parse local output address name to InetAddress object
			try {
				this.localOutputAddress = InetAddress.getByName(this.localOutputAddressS);
			} catch (UnknownHostException e) {
				String msg = "The host name ''{0}'' can not be evaluated to an internet address"; //$NON-NLS-1$
				msg = MessageFormat.format(msg, this.localOutputAddressS);
				throw new ConnectionException(msg, e);
			}

			// parse local output address port to integer
			try {
				this.localOutputPort = Integer.parseInt(localOutputPortS);
			} catch (NumberFormatException e) {
				throw ConnectionMessages.throwInvalidParameter(ClientSocketEndpoint.class,
						SocketEndpointConstants.PARAMETER_LOCAL_OUTPUT_PORT, localOutputPortS);
			}
		}

		// configure retry wait time
		String retryS = parameters.get(SocketEndpointConstants.PARAMETER_RETRY);
		if (retryS == null || retryS.length() == 0) {
			ConnectionMessages.warnUnsetParameter(ClientSocketEndpoint.class, SocketEndpointConstants.PARAMETER_RETRY,
					String.valueOf(SocketEndpointConstants.RETRY));
			this.retry = SocketEndpointConstants.RETRY;
		} else {
			try {
				this.retry = Long.parseLong(retryS);
			} catch (NumberFormatException e) {
				throw ConnectionMessages
						.throwInvalidParameter(ClientSocketEndpoint.class, SocketEndpointConstants.PARAMETER_RETRY,
								retryS);
			}
		}

		// configure connect on start
		String connectOnStartS = parameters.get(SocketEndpointConstants.PARAMETER_CONNECT_ON_START);
		if (StringHelper.isNotEmpty(connectOnStartS)) {
			this.connectOnStart = StringHelper.parseBoolean(connectOnStartS);
		} else {
			ConnectionMessages
					.warnUnsetParameter(ClientSocketEndpoint.class, SocketEndpointConstants.PARAMETER_CONNECT_ON_START,
							String.valueOf(SocketEndpointConstants.CONNECT_ON_START));
			this.connectOnStart = SocketEndpointConstants.CONNECT_ON_START;
		}

		// configure closeAfterSend
		String closeAfterSendS = parameters.get(SocketEndpointConstants.PARAMETER_CLOSE_AFTER_SEND);
		if (StringHelper.isNotEmpty(closeAfterSendS)) {
			this.closeAfterSend = StringHelper.parseBoolean(closeAfterSendS);
		} else {
			ConnectionMessages
					.warnUnsetParameter(ClientSocketEndpoint.class, SocketEndpointConstants.PARAMETER_CLOSE_AFTER_SEND,
							String.valueOf(SocketEndpointConstants.CLOSE_AFTER_SEND));
			this.closeAfterSend = SocketEndpointConstants.CLOSE_AFTER_SEND;
		}

		// configure if timeout on connection should be activated
		String useTimeoutS = parameters.get(SocketEndpointConstants.PARAMETER_USE_TIMEOUT);
		if (useTimeoutS == null || useTimeoutS.length() == 0) {
			ConnectionMessages
					.warnUnsetParameter(ClientSocketEndpoint.class, SocketEndpointConstants.PARAMETER_USE_TIMEOUT,
							String.valueOf(SocketEndpointConstants.USE_TIMEOUT));
			this.useTimeout = SocketEndpointConstants.USE_TIMEOUT;
		} else {
			this.useTimeout = Boolean.parseBoolean(useTimeoutS);
		}

		if (this.useTimeout) {
			// configure timeout on connection
			String timeoutS = parameters.get(SocketEndpointConstants.PARAMETER_TIMEOUT);
			if (timeoutS == null || timeoutS.length() == 0) {
				ConnectionMessages
						.warnUnsetParameter(ClientSocketEndpoint.class, SocketEndpointConstants.PARAMETER_TIMEOUT,
								String.valueOf(SocketEndpointConstants.TIMEOUT));
				this.timeout = SocketEndpointConstants.TIMEOUT;
			} else {
				try {
					this.timeout = Integer.parseInt(timeoutS);
				} catch (NumberFormatException e) {
					throw ConnectionMessages.throwInvalidParameter(ClientSocketEndpoint.class,
							SocketEndpointConstants.PARAMETER_TIMEOUT, timeoutS);
				}
			}
		}

		// configure if the connection should be cleared on connect
		String clearOnConnectS = parameters.get(SocketEndpointConstants.PARAMETER_CLEAR_ON_CONNECT);
		if (clearOnConnectS == null || clearOnConnectS.length() == 0) {
			ConnectionMessages
					.warnUnsetParameter(ClientSocketEndpoint.class, SocketEndpointConstants.PARAMETER_CLEAR_ON_CONNECT,
							String.valueOf(SocketEndpointConstants.CLEAR_ON_CONNECT));
			this.clearOnConnect = SocketEndpointConstants.CLEAR_ON_CONNECT;
		} else {
			this.clearOnConnect = Boolean.parseBoolean(clearOnConnectS);
		}
	}

	/**
	 * @return the uri as String to which this {@link ClientSocketEndpoint} is locally bound to
	 */
	@Override
	public String getLocalUri() {
		if (this.socket != null) {
			InetAddress localAddress = this.socket.getLocalAddress();
			return localAddress.getHostAddress() + StringHelper.COLON + this.socket.getLocalPort();
		} else if (this.localOutputAddress != null) {
			return this.localOutputAddress.getHostAddress() + StringHelper.COLON + this.localOutputPort;
		}

		return "0.0.0.0:0"; //$NON-NLS-1$
	}

	/**
	 * @return the uri as String to which this {@link ClientSocketEndpoint} is connecting to
	 */
	@Override
	public String getRemoteUri() {
		if (this.socket != null) {
			InetAddress remoteAddress = this.socket.getInetAddress();
			return remoteAddress.getHostAddress() + StringHelper.COLON + this.socket.getPort();
		} else if (this.remoteInputAddress != null) {
			return this.remoteInputAddress.getHostAddress() + StringHelper.COLON + this.remoteInputPort;
		}

		return this.remoteInputAddressS + StringHelper.COLON + this.remoteInputPort;
	}

	/**
	 * Allows this end point to connect and then opens the connection to the defined remote server
	 *
	 * @see CommunicationEndpoint#start()
	 */
	@Override
	public void start() {
		if (!this.closed) {
			logger.warn(MessageFormat
					.format("CommunicationConnection {0} already started.", this.connection.getId())); //$NON-NLS-1$
		} else {
			// logger.info(MessageFormat.format("Enabling connection {0}...", this.connection.getId())); //$NON-NLS-1$
			this.closed = false;
			this.connection.notifyStateChange(ConnectionState.INITIALIZED, ConnectionState.INITIALIZED.toString());
			if (this.connectOnStart) {
				openConnection();
			}
		}
	}

	/**
	 * Closes this connection and disallows this end point to reconnect
	 *
	 * @see CommunicationEndpoint#stop()
	 */
	@Override
	public void stop() {
		this.closed = true;

		closeConnection();
		this.connection.notifyStateChange(ConnectionState.DISCONNECTED, ConnectionState.DISCONNECTED.toString());

		logger.info(MessageFormat.format("Disabled connection {0}.", this.connection.getId())); //$NON-NLS-1$
	}

	@Override
	public void reset() {
		this.closed = true;
		closeConnection();
		this.connection.notifyStateChange(ConnectionState.INITIALIZED, ConnectionState.INITIALIZED.toString());
	}

	@Override
	public void simulate(IoMessage message) throws Exception {
		this.messageVisitor.simulate(message);
	}

	@Override
	public void send(IoMessage message) throws Exception {

		while (!this.closed && message.getState() == State.PENDING) {
			try {

				// open the connection
				if (!checkConnection())
					openConnection();

				// read and write to the client socket
				this.messageVisitor.visit(this.inputStream, this.outputStream, message);

				message.setState(State.DONE, State.DONE.name());

			} catch (Exception e) {
				if (this.closed) {
					logger.warn("Socket has been closed!"); //$NON-NLS-1$
					message.setState(State.FATAL, "Socket has been closed!"); //$NON-NLS-1$
				} else {
					closeConnection();
					logger.error(e.getMessage(), e);
					message.setState(State.FATAL, e.getLocalizedMessage());
					this.connection.notifyStateChange(ConnectionState.BROKEN, e.getLocalizedMessage());
				}
			} finally {
				if (this.closeAfterSend && !this.closed) {
					closeConnection();
				}
			}
		}
	}
}
