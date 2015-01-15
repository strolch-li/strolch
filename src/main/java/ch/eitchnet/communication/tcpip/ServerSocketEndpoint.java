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
package ch.eitchnet.communication.tcpip;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.net.BindException;
import java.net.InetAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketException;
import java.net.UnknownHostException;
import java.text.MessageFormat;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ch.eitchnet.communication.CommunicationConnection;
import ch.eitchnet.communication.CommunicationEndpoint;
import ch.eitchnet.communication.ConnectionException;
import ch.eitchnet.communication.ConnectionMessages;
import ch.eitchnet.communication.ConnectionState;
import ch.eitchnet.communication.IoMessage;
import ch.eitchnet.communication.IoMessageVisitor;
import ch.eitchnet.utils.helper.StringHelper;

/**
 * <p>
 * This {@link CommunicationEndpoint} is an abstract implementation with everything needed to start a {@link Socket}
 * server which waits for a request from a single client
 * </p>
 * <p>
 * This end point only allows a single connection at a time and implements all exception handling for opening and
 * closing a {@link Socket} connection
 * </p>
 * 
 * @see ServerSocketEndpoint#configure(CommunicationConnection, IoMessageVisitor) for details on configuring the end
 *      point
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class ServerSocketEndpoint implements CommunicationEndpoint, Runnable {

	protected static final Logger logger = LoggerFactory.getLogger(ServerSocketEndpoint.class);

	private Thread serverThread;

	// state variables
	private boolean connected;
	private boolean closed;
	private boolean fatal;
	private long lastConnect;
	private boolean useTimeout;
	private int timeout;
	private long retry;
	private boolean clearOnConnect;

	// address
	private String localInputAddressS;
	private int localInputPort;
	private String remoteOutputAddressS;
	private int remoteOutputPort;

	private InetAddress localInputAddress;
	private InetAddress remoteOutputAddress;

	// connection
	private ServerSocket serverSocket;
	private Socket socket;

	protected DataOutputStream outputStream;
	protected DataInputStream inputStream;

	protected CommunicationConnection connection;

	protected SocketMessageVisitor messageVisitor;

	/**
	 * Default constructor
	 */
	public ServerSocketEndpoint() {
		this.connected = false;
		this.closed = true;
		this.fatal = false;
	}

	/**
	 * Checks the state of the connection and returns true if {@link Socket} is connected and ready for transmission,
	 * false otherwise
	 * 
	 * @return true if {@link Socket} is connected and ready for transmission, false otherwise
	 */
	protected boolean checkConnection() {
		return !this.closed
				&& this.connected
				&& (this.socket != null && !this.socket.isClosed() && this.socket.isBound()
						&& this.socket.isConnected() && !this.socket.isInputShutdown() && !this.socket
							.isOutputShutdown());
	}

	/**
	 * Listens on the {@link ServerSocket} for an incoming connection. Prepares the connection then for use. If the
	 * remote address has been defined, then the remote connection is validated to come from this appropriate host.
	 * CommunicationConnection attempts are always separated by a configured amount of time
	 */
	protected void openConnection() {

		ConnectionState state = this.connection.getState();

		// do not open the connection if state is
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
					long wait = this.retry - timeDifference;
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
				String msg = "Waiting for connections on: {0}:{1}..."; //$NON-NLS-1$
				logger.info(MessageFormat.format(msg, this.localInputAddress.getHostAddress(),
						Integer.toString(this.localInputPort)));
				this.socket = this.serverSocket.accept();

				// validate that the remote side of the socket is really the client we want
				if (this.remoteOutputAddress != null) {

					String remoteAddr = this.socket.getInetAddress().getHostAddress();
					if (!remoteAddr.equals(this.remoteOutputAddress.getHostAddress())) {
						msg = "Illegal remote client at address {0}. Expected is {1}"; //$NON-NLS-1$
						msg = MessageFormat.format(msg, remoteAddr, this.remoteOutputAddress.getHostAddress());
						logger.error(msg);

						closeConnection();

						throw new ConnectionException(msg);
					}
				}

				// configure the socket
				if (logger.isDebugEnabled()) {
					msg = "BufferSize (send/read): {0} / {1} SoLinger: {2} TcpNoDelay: {3}"; //$NON-NLS-1$
					logger.debug(MessageFormat.format(msg, this.socket.getSendBufferSize(),
							this.socket.getReceiveBufferSize(), this.socket.getSoLinger(), this.socket.getTcpNoDelay()));
				}
				//inputSocket.setSendBufferSize(1);
				//inputSocket.setSoLinger(true, 0);
				//inputSocket.setTcpNoDelay(true);

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

				msg = "Connected {0}{1}: {2}:{3} with local side {4}:{5}"; //$NON-NLS-1$
				logger.info(MessageFormat.format(msg, this.getClass().getSimpleName(), this.connection.getId(),
						this.socket.getInetAddress().getHostName(), Integer.toString(this.socket.getPort()),
						this.socket.getLocalAddress().getHostAddress(), Integer.toString(this.socket.getLocalPort())));

				// we are connected!
				this.connection.notifyStateChange(ConnectionState.CONNECTED, ConnectionState.CONNECTED.toString());
				this.connected = true;

			} catch (InterruptedException e) {
				logger.warn("Interrupted!"); //$NON-NLS-1$
				this.closed = true;
				this.connection.notifyStateChange(ConnectionState.DISCONNECTED, null);
			} catch (Exception e) {
				if (this.closed && e instanceof SocketException) {
					logger.warn("Socket closed!"); //$NON-NLS-1$
					this.connection.notifyStateChange(ConnectionState.DISCONNECTED, null);
				} else {
					String msg = "Error while opening socket for inbound connection {0}"; //$NON-NLS-1$
					logger.error(MessageFormat.format(msg, this.connection.getId()));
					logger.error(e.getMessage(), e);
					this.connected = false;
					this.connection.notifyStateChange(ConnectionState.BROKEN, e.getLocalizedMessage());
				}
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
				logger.error(MessageFormat.format("Error closing OutputStream: {0}", e.getLocalizedMessage())); //$NON-NLS-1$
			} finally {
				this.outputStream = null;
			}
		}

		if (this.inputStream != null) {
			try {
				this.inputStream.close();
			} catch (IOException e) {
				logger.error(MessageFormat.format("Error closing InputStream: {0}", e.getLocalizedMessage())); //$NON-NLS-1$
			} finally {
				this.inputStream = null;
			}
		}

		if (this.socket != null) {
			try {
				this.socket.close();
			} catch (IOException e) {
				logger.error(MessageFormat.format("Error closing InputSocket: {0}", e.getLocalizedMessage())); //$NON-NLS-1$
			} finally {
				this.socket = null;
			}

			String msg = "Socket closed for inbound connection {0} at local input address {1}:{2}"; //$NON-NLS-1$
			logger.info(MessageFormat.format(msg, this.connection.getId(), this.localInputAddressS,
					Integer.toString(this.localInputPort)));
		}
	}

	/**
	 * <p>
	 * Configures this {@link ServerSocketEndpoint}
	 * </p>
	 * gets the parameter map from the connection and reads the following parameters from the map:
	 * <ul>
	 * <li>localInputAddress - the local IP or Hostname to bind to for incoming connections</li>
	 * <li>localInputPort - the local port on which to listen for incoming connections</li>
	 * <li>remoteOutputAddress - the IP or Hostname of the remote client. If this value is not null, then it will be
	 * verified that the connecting client is connecting from this address</li>
	 * <li>remoteOutputPort - the port from which the remote client must connect. If this value is not null, then it
	 * will be verified that the connecting client is connecting from this port</li>
	 * <li>retry - a configured retry wait time. Default is {@link SocketEndpointConstants#RETRY}</li>
	 * <li>timeout - the timeout after which an idle socket is deemed dead. Default is
	 * {@link SocketEndpointConstants#TIMEOUT}</li>
	 * <li>useTimeout - if true, then the timeout is activated. default is {@link SocketEndpointConstants#USE_TIMEOUT}</li>
	 * <li>clearOnConnect - if true, then the after a successful connect the input is cleared by discarding all
	 * available bytes. This can be useful in cases where the channel is clogged with stale data. default is
	 * {@link SocketEndpointConstants#CLEAR_ON_CONNECT}</li>
	 * </ul>
	 * 
	 * @see CommunicationEndpoint#configure(CommunicationConnection, IoMessageVisitor)
	 */
	@Override
	public void configure(CommunicationConnection connection, IoMessageVisitor messageVisitor) {
		if (this.connection != null && connection.getState().compareTo(ConnectionState.INITIALIZED) > 0) {
			logger.warn(MessageFormat.format("Inbound connection {0} already configured.", connection.getId())); //$NON-NLS-1$
			return;
		}

		ConnectionMessages.assertLegalMessageVisitor(this.getClass(), SocketMessageVisitor.class, messageVisitor);
		this.messageVisitor = (SocketMessageVisitor) messageVisitor;
		this.connection = connection;
		configure();
	}

	private void configure() {
		Map<String, String> parameters = this.connection.getParameters();

		this.localInputAddressS = parameters.get(SocketEndpointConstants.PARAMETER_LOCAL_INPUT_ADDRESS);
		String localInputPortS = parameters.get(SocketEndpointConstants.PARAMETER_LOCAL_INPUT_PORT);
		this.remoteOutputAddressS = parameters.get(SocketEndpointConstants.PARAMETER_REMOTE_OUTPUT_ADDRESS);
		String remoteOutputPortS = parameters.get(SocketEndpointConstants.PARAMETER_REMOTE_OUTPUT_PORT);

		// parse local Address to InetAddress object
		try {
			this.localInputAddress = InetAddress.getByName(this.localInputAddressS);
		} catch (UnknownHostException e) {
			throw ConnectionMessages.throwInvalidParameter(ServerSocketEndpoint.class,
					SocketEndpointConstants.PARAMETER_LOCAL_INPUT_ADDRESS, this.localInputAddressS);
		}

		// parse local address port to integer
		try {
			this.localInputPort = Integer.parseInt(localInputPortS);
		} catch (NumberFormatException e) {
			throw ConnectionMessages.throwInvalidParameter(ServerSocketEndpoint.class,
					SocketEndpointConstants.PARAMETER_LOCAL_INPUT_PORT, localInputPortS);
		}

		// if remote address is not set, then we will use the localhost InetAddress
		if (this.remoteOutputAddressS == null || this.remoteOutputAddressS.length() == 0) {
			logger.debug("No remoteOutputAddress set. Allowing connection from any remote address"); //$NON-NLS-1$
		} else {

			// parse remote output address name to InetAddress object
			try {
				this.remoteOutputAddress = InetAddress.getByName(this.remoteOutputAddressS);
			} catch (UnknownHostException e) {
				throw ConnectionMessages.throwInvalidParameter(ServerSocketEndpoint.class,
						SocketEndpointConstants.PARAMETER_REMOTE_OUTPUT_ADDRESS, this.remoteOutputAddressS);
			}

			// parse remote output address port to integer
			try {
				this.remoteOutputPort = Integer.parseInt(remoteOutputPortS);
			} catch (NumberFormatException e) {
				throw ConnectionMessages.throwInvalidParameter(ServerSocketEndpoint.class,
						SocketEndpointConstants.PARAMETER_REMOTE_OUTPUT_PORT, remoteOutputPortS);
			}
		}

		// configure retry wait time
		String retryS = parameters.get(SocketEndpointConstants.PARAMETER_RETRY);
		if (retryS == null || retryS.length() == 0) {
			ConnectionMessages.warnUnsetParameter(ServerSocketEndpoint.class, SocketEndpointConstants.PARAMETER_RETRY,
					String.valueOf(SocketEndpointConstants.RETRY));
			this.retry = SocketEndpointConstants.RETRY;
		} else {
			try {
				this.retry = Long.parseLong(retryS);
			} catch (NumberFormatException e) {
				throw ConnectionMessages.throwInvalidParameter(ServerSocketEndpoint.class,
						SocketEndpointConstants.PARAMETER_RETRY, retryS);
			}
		}

		// configure if timeout on connection should be activated
		String useTimeoutS = parameters.get(SocketEndpointConstants.PARAMETER_USE_TIMEOUT);
		if (useTimeoutS == null || useTimeoutS.length() == 0) {
			ConnectionMessages.warnUnsetParameter(ServerSocketEndpoint.class,
					SocketEndpointConstants.PARAMETER_USE_TIMEOUT, String.valueOf(SocketEndpointConstants.USE_TIMEOUT));
			this.useTimeout = SocketEndpointConstants.USE_TIMEOUT;
		} else {
			this.useTimeout = Boolean.parseBoolean(useTimeoutS);
		}

		if (this.useTimeout) {
			// configure timeout on connection
			String timeoutS = parameters.get(SocketEndpointConstants.PARAMETER_TIMEOUT);
			if (timeoutS == null || timeoutS.length() == 0) {
				ConnectionMessages.warnUnsetParameter(ServerSocketEndpoint.class,
						SocketEndpointConstants.PARAMETER_TIMEOUT, String.valueOf(SocketEndpointConstants.TIMEOUT));
				this.timeout = SocketEndpointConstants.TIMEOUT;
			} else {
				try {
					this.timeout = Integer.parseInt(timeoutS);
				} catch (NumberFormatException e) {
					throw ConnectionMessages.throwInvalidParameter(ServerSocketEndpoint.class,
							SocketEndpointConstants.PARAMETER_TIMEOUT, timeoutS);
				}
			}
		}

		// configure if the connection should be cleared on connect
		String clearOnConnectS = parameters.get(SocketEndpointConstants.PARAMETER_CLEAR_ON_CONNECT);
		if (clearOnConnectS == null || clearOnConnectS.length() == 0) {
			ConnectionMessages.warnUnsetParameter(ServerSocketEndpoint.class,
					SocketEndpointConstants.PARAMETER_CLEAR_ON_CONNECT,
					String.valueOf(SocketEndpointConstants.CLEAR_ON_CONNECT));
			this.clearOnConnect = SocketEndpointConstants.CLEAR_ON_CONNECT;
		} else {
			this.clearOnConnect = Boolean.parseBoolean(clearOnConnectS);
		}
	}

	/**
	 * @return the uri as String to which this {@link ServerSocketEndpoint} is locally bound to
	 */
	@Override
	public String getLocalUri() {
		if (this.socket != null) {
			InetAddress localAddress = this.socket.getLocalAddress();
			return localAddress.getHostAddress() + StringHelper.COLON + this.socket.getLocalPort();
		} else if (this.localInputAddress != null) {
			return this.localInputAddress.getHostAddress() + StringHelper.COLON + this.localInputPort;
		}

		return "0.0.0.0:0"; //$NON-NLS-1$
	}

	/**
	 * @return the uri as String from which this {@link ServerSocketEndpoint} is receiving data from
	 */
	@Override
	public String getRemoteUri() {
		if (this.socket != null) {
			InetAddress remoteAddress = this.socket.getInetAddress();
			return remoteAddress.getHostAddress() + StringHelper.COLON + this.socket.getPort();
		} else if (this.remoteOutputAddressS != null) {
			return this.remoteOutputAddress.getHostAddress() + StringHelper.COLON + this.remoteOutputPort;
		}

		return "0.0.0.0:0"; //$NON-NLS-1$
	}

	/**
	 * Starts the {@link Thread} to allow incoming connections
	 * 
	 * @see CommunicationEndpoint#start()
	 */
	@Override
	public void start() {

		if (this.fatal) {
			String msg = "CommunicationConnection had a fatal exception and can not yet be started. Please check log file for further information!"; //$NON-NLS-1$
			throw new ConnectionException(msg);
		}

		if (this.serverThread != null) {
			logger.warn(MessageFormat.format("CommunicationConnection {0} already started.", this.connection.getId())); //$NON-NLS-1$
		} else {
			// logger.info(MessageFormat.format("Enabling connection {0}...", this.connection.getId())); //$NON-NLS-1$
			this.closed = false;

			this.serverThread = new Thread(this, this.connection.getId());
			this.serverThread.start();
		}
	}

	/**
	 * Closes any open connection and then stops the {@link Thread} disallowing incoming connections
	 * 
	 * @see CommunicationEndpoint#stop()
	 */
	@Override
	public void stop() {
		closeThread();
		closeConnection();
		this.connection.notifyStateChange(ConnectionState.DISCONNECTED, ConnectionState.DISCONNECTED.toString());

		logger.info(MessageFormat.format("Disabled connection {0}.", this.connection.getId())); //$NON-NLS-1$
	}

	@Override
	public void reset() {
		closeThread();
		closeConnection();
		configure();
		this.connection.notifyStateChange(ConnectionState.INITIALIZED, ConnectionState.INITIALIZED.toString());
	}

	private void closeThread() {
		this.closed = true;
		this.fatal = false;

		if (this.serverThread != null) {
			try {
				this.serverThread.interrupt();
				if (this.serverSocket != null)
					this.serverSocket.close();
				this.serverThread.join(2000l);
			} catch (Exception e) {
				logger.error(MessageFormat.format(
						"Exception while interrupting server thread: {0}", e.getLocalizedMessage())); //$NON-NLS-1$
			}

			this.serverThread = null;
		}
	}

	/**
	 * Thread is listening on the ServerSocket and opens a new connection if necessary
	 */
	@Override
	public void run() {

		while (!this.closed) {

			// bomb-proof, catches all exceptions!
			try {

				// if serverSocket is null or closed, open a new server socket
				if (this.serverSocket == null || this.serverSocket.isClosed()) {

					try {
						//	String msg = "Opening socket on {0}:{1}..."; //$NON-NLS-1$
						//	logger.info(MessageFormat.format(msg, this.localInputAddress.getHostAddress(),
						//	Integer.toString(this.localInputPort)));
						this.serverSocket = new ServerSocket(this.localInputPort, 1, this.localInputAddress);
						this.serverSocket.setReuseAddress(true);
					} catch (BindException e) {
						logger.error("Fatal BindException occurred! Port is already in use, or address is illegal!"); //$NON-NLS-1$
						logger.error(e.getMessage(), e);
						this.closed = true;
						this.fatal = true;

						String msg = "Fatal error while binding to server socket. ServerSocket endpoint is dead"; //$NON-NLS-1$
						throw new ConnectionException(msg);
					}
				}

				// open the connection
				openConnection();

				// as long as connection is connected
				while (checkConnection()) {

					// read and write from the connected server socket
					IoMessage message = this.messageVisitor.visit(this.inputStream, this.outputStream);
					if (message != null) {
						this.connection.handleNewMessage(message);
					}
				}

			} catch (Exception e) {
				if (e instanceof InterruptedException) {
					logger.error("Interrupted!"); //$NON-NLS-1$
				} else {
					logger.error(e.getMessage(), e);
				}
				this.connection.notifyStateChange(ConnectionState.BROKEN, e.getLocalizedMessage());
			} finally {
				closeConnection();
			}
		}

		if (!this.fatal) {
			logger.warn(MessageFormat.format(
					"CommunicationConnection {0} is not running anymore!", this.connection.getId())); //$NON-NLS-1$
			this.connection.notifyStateChange(ConnectionState.BROKEN, null);
		} else {
			String msg = "CommunicationConnection {0} is broken due to a fatal exception!"; //$NON-NLS-1$
			logger.error(MessageFormat.format(msg, this.connection.getId()));
			this.connection.notifyStateChange(ConnectionState.DISCONNECTED, null);
		}
	}

	@Override
	public void simulate(IoMessage message) throws Exception {
		send(message);
	}

	@Override
	public void send(IoMessage message) throws Exception {
		String msg = "The Server Socket can not send messages, use the {0} implementation instead!"; //$NON-NLS-1$
		throw new UnsupportedOperationException(MessageFormat.format(msg, ClientSocketEndpoint.class.getName()));
	}
}
