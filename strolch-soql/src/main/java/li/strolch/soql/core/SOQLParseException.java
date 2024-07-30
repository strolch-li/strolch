package li.strolch.soql.core;

/**
 * @author msmock
 */
public class SOQLParseException extends RuntimeException {

	public SOQLParseException(String message) {
		super(message);
	}

	public SOQLParseException(String message, Throwable cause) {
		super(message, cause);
	}
}
