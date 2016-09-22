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
package li.strolch.communication.console;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import li.strolch.communication.CommunicationConnection;
import li.strolch.communication.CommunicationEndpoint;
import li.strolch.communication.ConnectionMessages;
import li.strolch.communication.ConnectionState;
import li.strolch.communication.IoMessage;
import li.strolch.communication.IoMessageVisitor;

/**
 * @author Robert von Burg &lt;eitch@eitchnet.ch&gt;
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

	@Override
	public void simulate(IoMessage message) throws Exception {
		send(message);
	}
}
