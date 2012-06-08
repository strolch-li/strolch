package ch.eitchnet.xmlpers;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class XmlPersistenceExecption extends RuntimeException {
	private static final long serialVersionUID = 1L;

	/**
	 * @param message
	 * @param cause
	 */
	public XmlPersistenceExecption(String message, Throwable cause) {
		super(message, cause);
	}

	/**
	 * @param message
	 */
	public XmlPersistenceExecption(String message) {
		super(message);
	}
}
