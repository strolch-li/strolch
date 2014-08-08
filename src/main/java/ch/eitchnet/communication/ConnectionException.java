package ch.eitchnet.communication;

/**
 * Base Exception for exceptions thrown by the communication classes
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class ConnectionException extends RuntimeException {
	private static final long serialVersionUID = 1L;

	public ConnectionException(String message, Throwable cause) {
		super(message, cause);
	}

	public ConnectionException(String message) {
		super(message);
	}
}
