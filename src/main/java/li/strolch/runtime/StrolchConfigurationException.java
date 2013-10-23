package li.strolch.runtime;

public class StrolchConfigurationException extends RuntimeException {

	private static final long serialVersionUID = 1L;

	public StrolchConfigurationException(String message) {
		super(message);
	}

	public StrolchConfigurationException(String message, Throwable cause) {
		super(message, cause);
	}
}
