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

import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.BlockingDeque;
import java.util.concurrent.LinkedBlockingDeque;

import li.strolch.communication.IoMessage.State;
import li.strolch.utils.collections.MapOfLists;
import li.strolch.utils.dbc.DBC;
import li.strolch.utils.helper.StringHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Robert von Burg &lt;eitch@eitchnet.ch&gt;
 */
public class CommunicationConnection implements Runnable {

	protected static final Logger logger = LoggerFactory.getLogger(CommunicationConnection.class);

	private String id;
	private ConnectionMode mode;
	private Map<String, String> parameters;

	private ConnectionState state;
	private String stateMsg;

	private BlockingDeque<IoMessage> messageQueue;
	private Thread queueThread;
	private volatile boolean run;
	private MapOfLists<CommandKey, ConnectionObserver> connectionObservers;
	private List<ConnectionStateObserver> connectionStateObservers;

	private CommunicationEndpoint endpoint;
	private IoMessageVisitor messageVisitor;

	private IoMessageArchive archive;

	public CommunicationConnection(String id, ConnectionMode mode, Map<String, String> parameters,
			CommunicationEndpoint endpoint, IoMessageVisitor messageVisitor) {

		DBC.PRE.assertNotEmpty("Id must be set!", id); //$NON-NLS-1$
		DBC.PRE.assertNotNull("ConnectionMode must be set!", mode); //$NON-NLS-1$
		DBC.PRE.assertNotNull("Paramerters must not be null!", parameters); //$NON-NLS-1$
		DBC.PRE.assertNotNull("Endpoint must be set!", endpoint); //$NON-NLS-1$
		DBC.PRE.assertNotNull("IoMessageVisitor must be set!", messageVisitor); //$NON-NLS-1$

		this.id = id;
		this.mode = mode;
		this.parameters = parameters;
		this.endpoint = endpoint;
		this.messageVisitor = messageVisitor;

		this.state = ConnectionState.CREATED;
		this.stateMsg = this.state.toString();
		this.messageQueue = new LinkedBlockingDeque<>();
		this.connectionObservers = new MapOfLists<>();
		this.connectionStateObservers = new ArrayList<>();
	}

	public void setArchive(IoMessageArchive archive) {
		this.archive = archive;
	}

	public IoMessageArchive getArchive() {
		return this.archive;
	}

	public String getId() {
		return this.id;
	}

	public int getQueueSize() {
		return this.messageQueue.size();
	}

	public ConnectionState getState() {
		return this.state;
	}

	public String getStateMsg() {
		return this.stateMsg;
	}

	public ConnectionMode getMode() {
		return this.mode;
	}

	public Map<String, String> getParameters() {
		return this.parameters;
	}

	public void clearQueue() {
		this.messageQueue.clear();
	}

	public void addConnectionObserver(CommandKey key, ConnectionObserver observer) {
		synchronized (this.connectionObservers) {
			this.connectionObservers.addElement(key, observer);
		}
	}

	public void removeConnectionObserver(CommandKey key, ConnectionObserver observer) {
		synchronized (this.connectionObservers) {
			this.connectionObservers.removeElement(key, observer);
		}
	}

	public void addConnectionStateObserver(ConnectionStateObserver observer) {
		synchronized (this.connectionStateObservers) {
			this.connectionStateObservers.add(observer);
		}
	}

	public void removeConnectionStateObserver(ConnectionStateObserver observer) {
		synchronized (this.connectionStateObservers) {
			this.connectionStateObservers.remove(observer);
		}
	}

	public void notifyStateChange(ConnectionState state, String stateMsg) {
		ConnectionState oldState = this.state;
		String oldStateMsg = this.stateMsg;
		this.state = state;
		this.stateMsg = stateMsg;

		List<ConnectionStateObserver> observers;
		synchronized (this.connectionStateObservers) {
			observers = new ArrayList<>(this.connectionStateObservers);
		}
		for (ConnectionStateObserver observer : observers) {
			observer.notify(oldState, oldStateMsg, state, stateMsg);
		}
	}

	public void switchMode(ConnectionMode mode) {
		ConnectionMessages.assertConfigured(this, "Can not switch modes yet!"); //$NON-NLS-1$
		if (mode == ConnectionMode.OFF) {
			stop();
		} else if (mode == ConnectionMode.ON) {
			stop();
			start();
		}

		this.mode = mode;
	}

	/**
	 * Configure the underlying {@link CommunicationEndpoint} and {@link IoMessageVisitor}
	 */
	public void configure() {
		this.messageVisitor.configure(this);
		this.endpoint.configure(this, this.messageVisitor);
		notifyStateChange(ConnectionState.INITIALIZED, ConnectionState.INITIALIZED.name());
	}

	public void start() {
		ConnectionMessages.assertConfigured(this, "Can not start yet!"); //$NON-NLS-1$

		switch (this.mode) {
		case OFF:
			logger.info("Not connecting as mode is currently OFF"); //$NON-NLS-1$
			break;
		case SIMULATION:
			logger.info("Started SIMULATION connection!"); //$NON-NLS-1$
			break;
		case ON:
			if (this.queueThread != null) {
				logger.warn(MessageFormat.format("{0}: Already connected!", this.id)); //$NON-NLS-1$
			} else {
				logger.info(MessageFormat
						.format("Starting Connection {0} to {1}...", this.id, getRemoteUri())); //$NON-NLS-1$
				this.run = true;
				this.queueThread = new Thread(this, MessageFormat.format("{0}_OUT", this.id)); //$NON-NLS-1$
				this.queueThread.start();

				connectEndpoint();
			}
			break;
		default:
			logger.error("Unhandled mode " + this.mode); //$NON-NLS-1$
			break;
		}
	}

	public void stop() {
		ConnectionMessages.assertConfigured(this, "Can not stop yet!"); //$NON-NLS-1$

		switch (this.mode) {
		case OFF:
			break;
		case SIMULATION:
			logger.info("Disconnected SIMULATION connection!"); //$NON-NLS-1$
			break;
		case ON:
			logger.info("Disconnecting..."); //$NON-NLS-1$
			if (this.queueThread == null) {
				logger.warn(MessageFormat.format("{0}: Already disconnected!", this.id)); //$NON-NLS-1$
			} else {
				this.run = false;

				try {
					disconnectEndpoint();
				} catch (Exception e) {
					String msg = "Caught exception while disconnecting endpoint: {0}"; //$NON-NLS-1$
					logger.error(MessageFormat.format(msg, e.getLocalizedMessage()), e);
				}

				try {
					this.queueThread.interrupt();
				} catch (Exception e) {
					String msg = "Caught exception while stopping queue thread: {0}"; //$NON-NLS-1$
					logger.warn(MessageFormat.format(msg, e.getLocalizedMessage()));
				}
				String msg = "{0} is stopped"; //$NON-NLS-1$
				logger.info(MessageFormat.format(msg, this.queueThread.getName()));
				this.queueThread = null;
			}
			break;
		default:
			logger.error("Unhandled mode " + this.mode); //$NON-NLS-1$
			break;
		}
	}

	/**
	 * Called by the underlying endpoint when a new message has been received and parsed
	 *
	 * @param message
	 * 		the message to handle
	 */
	public void handleNewMessage(IoMessage message) {
		ConnectionMessages.assertConfigured(this, "Can not be notified of new message yet!"); //$NON-NLS-1$
		logger.info("Received new message " + message.getKey() + " " + message.getId() + " " + message.getState());

		// if the state of the message is already later than ACCEPTED 
		// then an underlying component has already set the state, so 
		// we don't need to set it
		if (message.getState().compareTo(State.ACCEPTED) < 0)
			message.setState(State.ACCEPTED, StringHelper.DASH);

		notifyObservers(message);
	}

	public void notifyObservers(IoMessage message) {

		List<ConnectionObserver> observers;
		synchronized (this.connectionObservers) {
			List<ConnectionObserver> list = this.connectionObservers.getList(message.getKey());
			if (list == null || list.isEmpty()) {
				logger.info("No observers waiting for key " + message.getKey());
				return;
			}

			observers = new ArrayList<>(list);
		}

		for (ConnectionObserver observer : observers) {
			try {
				logger.info("Notifying observer " + observer.getClass().getSimpleName());
				observer.notify(message.getKey(), message);
			} catch (Exception e) {
				String msg = "Failed to notify observer for key {0} on message with id {1}"; //$NON-NLS-1$
				logger.error(MessageFormat.format(msg, message.getKey(), message.getId()), e);
			}
		}

		if (this.archive != null)
			this.archive.archive(message);
	}

	@Override
	public void run() {
		while (this.run) {

			IoMessage message = null;

			try {

				message = this.messageQueue.take();
				logger.info(MessageFormat.format("Processing message {0}...", message.getId())); //$NON-NLS-1$

				if (this.mode == ConnectionMode.ON)
					this.endpoint.send(message);
				else if (this.mode == ConnectionMode.SIMULATION)
					this.endpoint.simulate(message);

				// notify the caller that the message has been processed
				if (message.getState().compareTo(State.DONE) < 0)
					message.setState(State.DONE, StringHelper.DASH);

				done(message);

			} catch (InterruptedException e) {
				logger.warn(MessageFormat.format("{0} connection has been interruped!", this.id)); //$NON-NLS-1$

				// an interrupted exception means the thread must stop
				this.run = false;

				if (message != null) {
					logger.error(MessageFormat.format("Can not send message {0}", message.getId())); //$NON-NLS-1$
					message.setState(State.FATAL, e.getLocalizedMessage());
					done(message);
				}

			} catch (Exception e) {
				logger.error(e.getMessage(), e);

				if (message != null) {
					logger.error(MessageFormat.format("Can not send message {0}", message.getId())); //$NON-NLS-1$
					message.setState(State.FATAL, e.getLocalizedMessage());
					done(message);
				}
			} finally {
				if (message != null && this.archive != null) {
					this.archive.archive(message);
				}
			}
		}
	}

	/**
	 * Called when an outgoing message has been handled. This method logs the message state and then notifies all
	 * observers
	 *
	 * @param message
	 * 		the message which is done
	 */
	public void done(IoMessage message) {
		ConnectionMessages.assertConfigured(this, "Can not notify observers yet!"); //$NON-NLS-1$

		switch (message.getState()) {
		case ACCEPTED:
		case CREATED:
		case DONE:
		case PENDING:
			logger.info(MessageFormat.format("Sent message {0}", message.toString())); //$NON-NLS-1$
			break;
		case FAILED:
		case FATAL:
			logger.error(MessageFormat.format("Failed to send message {0}", message.toString())); //$NON-NLS-1$
			break;
		default:
			logger.error(MessageFormat.format("Unhandled state for message {0}", message.toString())); //$NON-NLS-1$
			break;
		}

		notifyObservers(message);
	}

	public String getRemoteUri() {
		return this.endpoint == null ? "0.0.0.0:0" : this.endpoint.getRemoteUri(); //$NON-NLS-1$
	}

	public String getLocalUri() {
		return this.endpoint == null ? "0.0.0.0:0" : this.endpoint.getLocalUri(); //$NON-NLS-1$
	}

	public void reset() {
		ConnectionMessages.assertConfigured(this, "Can not resest yet!"); //$NON-NLS-1$
		this.endpoint.reset();
	}

	/**
	 * Called when the connection is connected, thus the underlying endpoint can be started
	 */
	protected void connectEndpoint() {
		this.endpoint.start();
	}

	/**
	 * Called when the connection is disconnected, thus the underlying endpoint must be stopped
	 */
	protected void disconnectEndpoint() {
		this.endpoint.stop();
	}

	/**
	 * Send the message using the underlying endpoint. Do not change the state of the message, this will be done by the
	 * caller
	 *
	 * @param message
	 * 		the message to send
	 */
	public void send(IoMessage message) {
		ConnectionMessages.assertConfigured(this, "Can not send yet"); //$NON-NLS-1$
		if (this.mode == ConnectionMode.OFF)
			throw ConnectionMessages.throwNotConnected(this, message);

		message.setState(State.PENDING, State.PENDING.name());

		this.messageQueue.add(message);
	}
}
