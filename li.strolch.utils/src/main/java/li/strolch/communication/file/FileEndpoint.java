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
package li.strolch.communication.file;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.text.MessageFormat;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import li.strolch.communication.CommunicationConnection;
import li.strolch.communication.CommunicationEndpoint;
import li.strolch.communication.ConnectionException;
import li.strolch.communication.ConnectionMessages;
import li.strolch.communication.ConnectionState;
import li.strolch.communication.IoMessage;
import li.strolch.communication.IoMessageVisitor;
import li.strolch.communication.StreamMessageVisitor;
import li.strolch.utils.helper.StringHelper;

/**
 * An {@link CommunicationEndpoint} which writes and/or reads from a designated file
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class FileEndpoint implements CommunicationEndpoint, Runnable {

	public static final String ENDPOINT_MODE = "endpointMode"; //$NON-NLS-1$
	public static final String INBOUND_FILENAME = "inboundFilename"; //$NON-NLS-1$
	public static final String OUTBOUND_FILENAME = "outboundFilename"; //$NON-NLS-1$
	public static final long POLL_TIME = 1000l;

	private static final Logger logger = LoggerFactory.getLogger(FileEndpoint.class);

	private CommunicationConnection connection;

	private FileEndpointMode endpointMode;
	private String inboundFilename;
	private String outboundFilename;
	private Thread thread;
	private boolean run = false;
	private StreamMessageVisitor messageVisitor;

	/**
	 * {@link FileEndpoint} needs the following parameters on the configuration to be initialized
	 * <ul>
	 * <li>outboundFilename: the file name where the {@link IoMessage} contents are written to. The value may contain
	 * {@link System#getProperty(String)} place holders which will be evaluated</li>
	 * </ul>
	 */
	@Override
	public void configure(CommunicationConnection connection, IoMessageVisitor messageVisitor) {
		this.connection = connection;

		ConnectionMessages.assertLegalMessageVisitor(this.getClass(), StreamMessageVisitor.class, messageVisitor);
		this.messageVisitor = (StreamMessageVisitor) messageVisitor;

		configure();
	}

	private void configure() {
		Map<String, String> parameters = this.connection.getParameters();

		String endpointModeS = parameters.get(ENDPOINT_MODE);
		if (StringHelper.isEmpty(endpointModeS)) {
			throw ConnectionMessages.throwInvalidParameter(FileEndpoint.class, ENDPOINT_MODE, endpointModeS);
		}
		try {
			this.endpointMode = FileEndpointMode.valueOf(endpointModeS);
		} catch (Exception e) {
			throw ConnectionMessages.throwInvalidParameter(FileEndpoint.class, ENDPOINT_MODE, endpointModeS);
		}

		if (this.endpointMode.isRead()) {
			this.inboundFilename = parameters.get(INBOUND_FILENAME);
			if (StringHelper.isEmpty(this.inboundFilename)) {
				throw ConnectionMessages.throwInvalidParameter(FileEndpoint.class, INBOUND_FILENAME,
						this.inboundFilename);
			}
		}

		if (this.endpointMode.isWrite()) {
			this.outboundFilename = parameters.get(OUTBOUND_FILENAME);
			if (StringHelper.isEmpty(this.outboundFilename)) {
				throw ConnectionMessages.throwInvalidParameter(FileEndpoint.class, OUTBOUND_FILENAME,
						this.outboundFilename);
			}
		}
	}

	@Override
	public String getLocalUri() {
		return new File(this.inboundFilename).getAbsolutePath();
	}

	@Override
	public String getRemoteUri() {
		return new File(this.outboundFilename).getAbsolutePath();
	}

	@Override
	public void start() {
		if (this.endpointMode.isRead()) {
			this.thread = new Thread(this, new File(this.inboundFilename).getName());
			this.run = true;
			this.thread.start();
		}
		this.connection.notifyStateChange(ConnectionState.IDLE, ConnectionState.IDLE.toString());
	}

	@Override
	public void stop() {
		stopThread();
		this.connection.notifyStateChange(ConnectionState.DISCONNECTED, ConnectionState.DISCONNECTED.toString());
	}

	@Override
	public void reset() {
		stopThread();
		configure();
		this.connection.notifyStateChange(ConnectionState.INITIALIZED, ConnectionState.INITIALIZED.toString());
	}

	private void stopThread() {
		this.run = false;
		if (this.thread != null) {
			try {
				this.thread.interrupt();
				this.thread.join(2000l);
			} catch (Exception e) {
				logger.error(MessageFormat.format("Error while interrupting thread: {0}", e.getLocalizedMessage())); //$NON-NLS-1$
			}

			this.thread = null;
		}
	}

	@Override
	public void send(IoMessage message) throws Exception {
		if (!this.endpointMode.isWrite()) {
			String msg = "FileEnpoint mode is {0} and thus write is not allowed!"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, this.endpointMode);
			throw new ConnectionException(msg);
		}

		this.connection.notifyStateChange(ConnectionState.WORKING, ConnectionState.WORKING.toString());

		// open the stream
		try (FileOutputStream outputStream = new FileOutputStream(this.outboundFilename, false)) {

			// write the message using the visitor
			this.messageVisitor.visit(outputStream, message);

		} finally {
			this.connection.notifyStateChange(ConnectionState.IDLE, ConnectionState.IDLE.toString());
		}
	}

	@Override
	public void simulate(IoMessage message) throws Exception {
		this.messageVisitor.simulate(message);
	}

	@Override
	public void run() {

		File file = new File(this.inboundFilename);

		long lastModified = 0l;

		logger.info("Starting..."); //$NON-NLS-1$
		while (this.run) {

			try {

				if (file.canRead()) {
					long tmpModified = file.lastModified();
					if (tmpModified > lastModified) {

						logger.info(MessageFormat.format("Handling file {0}", file.getAbsolutePath())); //$NON-NLS-1$

						this.connection.notifyStateChange(ConnectionState.WORKING, ConnectionState.WORKING.toString());

						// file is changed
						lastModified = tmpModified;

						// read the file
						handleFile(file);

						this.connection.notifyStateChange(ConnectionState.IDLE, ConnectionState.IDLE.toString());
					}
				}

				if (this.run) {
					this.connection.notifyStateChange(ConnectionState.WAITING, ConnectionState.WAITING.toString());
					try {
						synchronized (this) {
							this.wait(POLL_TIME);
						}
					} catch (InterruptedException e) {
						this.run = false;
						logger.info("Interrupted!"); //$NON-NLS-1$
					}
				}

			} catch (Exception e) {
				logger.error(MessageFormat.format("Error reading file: {0}", file.getAbsolutePath())); //$NON-NLS-1$
				logger.error(e.getMessage(), e);

				this.connection.notifyStateChange(ConnectionState.BROKEN, e.getLocalizedMessage());
			}
		}
	}

	/**
	 * Reads the file and handle using {@link StreamMessageVisitor}
	 * 
	 * @param file
	 *            the {@link File} to read
	 */
	protected void handleFile(File file) throws Exception {

		try (InputStream inputStream = new FileInputStream(file)) {

			// convert the object to an integration message
			IoMessage message = this.messageVisitor.visit(inputStream);

			// and forward to the connection
			if (message != null) {
				this.connection.handleNewMessage(message);
			}
		}
	}
}
