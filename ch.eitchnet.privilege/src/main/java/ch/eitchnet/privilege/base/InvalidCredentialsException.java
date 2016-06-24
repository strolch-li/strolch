package ch.eitchnet.privilege.base;

/**
 * Exception thrown if the given credentials are invalid
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class InvalidCredentialsException extends AccessDeniedException {

	private static final long serialVersionUID = 1L;

	/**
	 * @param msg
	 *            the message to accompany the exception
	 */
	public InvalidCredentialsException(String msg) {
		super(msg);
	}
}
