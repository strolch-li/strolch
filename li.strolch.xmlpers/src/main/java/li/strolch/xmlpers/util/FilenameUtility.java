/*
 * Copyright 2013 Robert von Burg <eitch@eitchnet.ch>
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *     http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package li.strolch.xmlpers.util;

import java.text.MessageFormat;

import li.strolch.xmlpers.api.XmlPersistenceException;
import li.strolch.xmlpers.impl.PathBuilder;

public class FilenameUtility {

	public static String getId(String filename) {
		assertFilename(filename);
		return filename.substring(0, filename.length() - PathBuilder.EXT_LENGTH);
	}

	public static void assertFilename(String filename) {
		if (filename.charAt(filename.length() - PathBuilder.EXT_LENGTH) != '.') {
			String msg = "The filename does not have a . (dot) at index {0}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, (filename.length() - PathBuilder.EXT_LENGTH));
			throw new XmlPersistenceException(msg);
		}
	}

}
