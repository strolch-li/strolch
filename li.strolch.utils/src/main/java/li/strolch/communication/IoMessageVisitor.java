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

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import li.strolch.communication.console.ConsoleMessageVisitor;

/**
 * <p>
 * Visitors to read and write {@link IoMessage} using different kind of endpoints. Different endpoints will require
 * different ways of writing or reading message, thus this is not defined here. Known extensions are
 * {@link ConsoleMessageVisitor}, {@link StreamMessageVisitor}.
 * </p>
 * 
 * <p>
 * Concrete implementations must be thread safe!
 * </p>
 * 
 * @author Robert von Burg &lt;eitch@eitchnet.ch&gt;
 */
public abstract class IoMessageVisitor {

	protected static final Logger logger = LoggerFactory.getLogger(IoMessageVisitor.class);

	protected CommunicationConnection connection;

	public void configure(CommunicationConnection connection) {
		this.connection = connection;
	}

	public void simulate(IoMessage ioMessage) {
		// allow for subclasses to implement
	}
}
