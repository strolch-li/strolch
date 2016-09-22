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
package li.strolch.utils.helper;

import java.text.MessageFormat;

/**
 * Utility class for working with {@link Class Classes}
 * 
 * @author Robert von Burg &lt;eitch@eitchnet.ch&gt;
 */
public class ClassHelper {

	/**
	 * Returns an instance of the class' name given by instantiating the class through an empty arguments constructor
	 * 
	 * @param <T>
	 *            the type of the class to return
	 * @param className
	 *            the name of a class to instantiate through an empty arguments constructor
	 * 
	 * @return the newly instantiated object from the given class name
	 * 
	 * @throws IllegalArgumentException
	 *             if the class could not be instantiated
	 */
	@SuppressWarnings("unchecked")
	public static <T> T instantiateClass(String className) throws IllegalArgumentException {
		try {

			Class<T> clazz = (Class<T>) Class.forName(className);

			return clazz.getConstructor().newInstance();

		} catch (Exception e) {
			String msg = MessageFormat.format("The class {0} could not be instantiated: ", className); //$NON-NLS-1$
			throw new IllegalArgumentException(msg, e);
		}
	}

	/**
	 * Instantiates an object for the given {@link Class} using an empty arguments constructor
	 * 
	 * @param <T>
	 *            the type of the class to return
	 * @param clazz
	 *            the {@link Class} from which a new object is to be instantiated using an empty arguments constructor
	 * 
	 * @return the newly instantiated object from the given {@link Class}
	 * 
	 * @throws IllegalArgumentException
	 *             if the {@link Class} could not be instantiated
	 */
	public static <T> T instantiateClass(Class<T> clazz) throws IllegalArgumentException {
		try {

			return clazz.getConstructor().newInstance();

		} catch (Exception e) {
			String msg = MessageFormat.format("The class {0} could not be instantiated: ", clazz.getName()); //$NON-NLS-1$
			throw new IllegalArgumentException(msg, e);
		}
	}

	/**
	 * Loads the {@link Class} object for the given class name
	 * 
	 * @param <T>
	 *            the type of {@link Class} to return
	 * @param className
	 *            the name of the {@link Class} to load and return
	 * 
	 * @return the {@link Class} object for the given class name
	 * 
	 * @throws IllegalArgumentException
	 *             if the class could not be instantiated
	 */
	@SuppressWarnings("unchecked")
	public static <T> Class<T> loadClass(String className) throws IllegalArgumentException {
		try {

			Class<T> clazz = (Class<T>) Class.forName(className);

			return clazz;

		} catch (Exception e) {
			String msg = MessageFormat.format("The class {0} could not be instantiated: ", className); //$NON-NLS-1$
			throw new IllegalArgumentException(msg, e);
		}
	}
}