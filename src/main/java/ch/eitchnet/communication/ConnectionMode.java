package ch.eitchnet.communication;

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
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public enum ConnectionMode {

	/**
	 * Denotes that the {@link CommunicationConnection} is off. This means it cannot accept messages, process messages
	 * or do any other kind of work
	 */
	OFF,

	/**
	 * Denotes that the {@link CommunicationConnection} is on. This means that the {@link CommunicationConnection}
	 * accepts and process messages. Any connections which need to be established will automatically be connected and
	 * re-established should an {@link IOException} occur
	 */
	ON,

	/**
	 * Denotes that the {@link CommunicationConnection} is in simulation mode. Mostly this means that the
	 * {@link CommunicationConnection} accepts messages, but silently swallows them, instead of processing them
	 */
	SIMULATION;
}
