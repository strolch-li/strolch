package ch.eitchnet.utils.helper;

import java.text.MessageFormat;

/**
 * Utility class for working with {@link Class Classes}
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class ClassHelper {

	@SuppressWarnings("unchecked")
	public static <T> T instantiateClass(String className) {

		try {
			Class<?> clazz = Class.forName(className);
			return (T) clazz.newInstance();
		} catch (Exception e) {
			String msg = "Failed to load class {0} due to error: {1}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, className, e.getMessage());
			throw new IllegalArgumentException(msg);
		}
	}
}
