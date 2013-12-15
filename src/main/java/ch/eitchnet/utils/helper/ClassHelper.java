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
