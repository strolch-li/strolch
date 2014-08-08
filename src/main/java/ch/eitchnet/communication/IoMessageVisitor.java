package ch.eitchnet.communication;

import ch.eitchnet.communication.console.ConsoleMessageVisitor;

/**
 * <p>
 * Visitors to read and write {@link IoMessage} using different kind of endpoints. Different entpoints will require
 * different ways of writing or reading message, thus this is not defined here. Known extensions are
 * {@link ConsoleMessageVisitor}, {@link StreamMessageVisitor}.
 * </p>
 * 
 * <p>
 * Concrete implementations must be thread safe!
 * </p>
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public abstract class IoMessageVisitor {

	protected CommunicationConnection connection;

	public void configure(CommunicationConnection connection) {
		this.connection = connection;
	}
}
