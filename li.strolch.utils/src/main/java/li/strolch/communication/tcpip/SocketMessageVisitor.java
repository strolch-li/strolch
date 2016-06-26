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
import java.net.Socket;

import li.strolch.communication.IoMessage;
import li.strolch.communication.IoMessageVisitor;

/**
 * This {@link IoMessageVisitor} implements and endpoint connecting to a {@link Socket}.
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public abstract class SocketMessageVisitor extends IoMessageVisitor {

	protected final String connectionId;

	public SocketMessageVisitor(String connectionId) {
		this.connectionId = connectionId;
	}

	public String getConnectionId() {
		return this.connectionId;
	}

	/**
	 * This method is called when a message is read from the underlying {@link Socket}
	 * 
	 * @param inputStream
	 * @param outputStream
	 * @return
	 * @throws Exception
	 */
	public abstract IoMessage visit(DataInputStream inputStream, DataOutputStream outputStream) throws Exception;

	/**
	 * This method is called when a message is to be sent to the underlying connected endpoint
	 * 
	 * @param inputStream
	 * @param outputStream
	 * @param message
	 * @throws Exception
	 */
	public abstract void visit(DataInputStream inputStream, DataOutputStream outputStream, IoMessage message)
			throws Exception;
}
