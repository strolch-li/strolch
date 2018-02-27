package li.strolch.soql.core;

/**
 * @author msmock
 */
public class SOQLParseException extends RuntimeException {

	private static final long serialVersionUID = 1L;

	public SOQLParseException(String message) {
		super(message);
	}

	public SOQLParseException(String message, Throwable cause) {
		super(message, cause);
	}
}
