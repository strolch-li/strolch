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

import java.io.IOException;

/**
 * <p>
 * The mode of an {@link CommunicationConnection} can be one of the following enum values. This makes it possible use
 * the connection without starting connection and later starting the connection when required
 * </p>
 * The modes have the following semantics:
 * <ul>
 * <li>OFF - the connection can only have states {@link ConnectionState#CREATED} and {@link ConnectionState#INITIALIZED}
 * . Trying to use the connection will throw an exception</li>
 * <li>ON - the connection can be used normally</li>
 * <li>SIMULATION - the same as ON, with the difference that the connection should silently drop any work</li>
 * </ul>
 * 
 * @author Robert von Burg &lt;eitch@eitchnet.ch&gt;
 */
public enum ConnectionMode {

	/**
	 * Denotes that the {@link CommunicationConnection} is off. This means it cannot accept messages, process messages
	 * or do any other kind of work
	 */
	OFF {
		@Override
		public boolean isSimulation() {
			return false;
		}
	},

	/**
	 * Denotes that the {@link CommunicationConnection} is on. This means that the {@link CommunicationConnection}
	 * accepts and process messages. Any connections which need to be established will automatically be connected and
	 * re-established should an {@link IOException} occur
	 */
	ON {
		@Override
		public boolean isSimulation() {
			return false;
		}
	},

	/**
	 * Denotes that the {@link CommunicationConnection} is in simulation mode. Mostly this means that the
	 * {@link CommunicationConnection} accepts messages, but silently swallows them, instead of processing them
	 */
	SIMULATION {
		@Override
		public boolean isSimulation() {
			return true;
		}
	};

	/**
	 * @return true if the current mode is simulation, false otherwise
	 */
	public abstract boolean isSimulation();
}
