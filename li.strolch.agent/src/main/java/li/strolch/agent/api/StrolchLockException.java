package li.strolch.agent.api;

import li.strolch.exception.StrolchException;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class StrolchLockException extends StrolchException {

	private static final long serialVersionUID = 1L;

	public StrolchLockException(String message, Throwable cause) {
		super(message, cause);
	}

	public StrolchLockException(String message) {
		super(message);
	}
}
