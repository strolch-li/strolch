package li.strolch.runtime.configuration;

public class StrolchConfigurationException extends RuntimeException {

	private static final long serialVersionUID = 1L;

	public StrolchConfigurationException(String message) {
		super(message);
	}

	public StrolchConfigurationException(String message, Throwable cause) {
		super(message, cause);
	}
}
