package li.strolch.exception;

public class StrolchElementNotFoundException extends StrolchModelException {

	public StrolchElementNotFoundException(String message, Throwable cause) {
		super(message, cause);
	}

	public StrolchElementNotFoundException(String message) {
		super(message);
	}
}
