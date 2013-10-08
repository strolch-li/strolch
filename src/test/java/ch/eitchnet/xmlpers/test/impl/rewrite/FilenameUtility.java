package ch.eitchnet.xmlpers.test.impl.rewrite;

import java.text.MessageFormat;

import ch.eitchnet.xmlpers.api.XmlPersistenceException;
import ch.eitchnet.xmlpers.impl.XmlPersistencePathBuilder;

public class FilenameUtility {

	public static String getId(String filename) {
		assertFilename(filename);
		return filename.substring(0, filename.length() - XmlPersistencePathBuilder.EXT_LENGTH);
	}

	public static void assertFilename(String filename) {
		if (filename.charAt(filename.length() - XmlPersistencePathBuilder.EXT_LENGTH) != '.') {
			String msg = "The filename does not have a . (dot) at index {0}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, (filename.length() - XmlPersistencePathBuilder.EXT_LENGTH));
			throw new XmlPersistenceException(msg);
		}
	}

}
