/*
 * Copyright (c) 2010 - 2012
 * 
 * This file is part of Privilege.
 *
 * Privilege is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Privilege is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with Privilege.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */
package ch.eitchnet.privilege.helper;

import ch.eitchnet.privilege.i18n.PrivilegeException;

/**
 * The {@link ClassHelper} class is a helper to instantiate classes using reflection
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
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
	 * @throws PrivilegeException
	 *             if the class could not be instantiated
	 */
	@SuppressWarnings("unchecked")
	public static <T> T instantiateClass(String className) throws PrivilegeException {
		try {

			Class<T> clazz = (Class<T>) Class.forName(className);

			return clazz.getConstructor().newInstance();

		} catch (Exception e) {
			throw new PrivilegeException("The class " + className + " could not be instantiated: ", e);
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
	 * @throws PrivilegeException
	 *             if the {@link Class} could not be instantiated
	 */
	public static <T> T instantiateClass(Class<T> clazz) throws PrivilegeException {
		try {

			return clazz.getConstructor().newInstance();

		} catch (Exception e) {
			throw new PrivilegeException("The class " + clazz.getName() + " could not be instantiated: ", e);
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
	 * @throws PrivilegeException
	 *             if the class could not be instantiated
	 */
	@SuppressWarnings("unchecked")
	public static <T> Class<T> loadClass(String className) throws PrivilegeException {
		try {

			Class<T> clazz = (Class<T>) Class.forName(className);

			return clazz;

		} catch (Exception e) {
			throw new PrivilegeException("The class " + className + " could not be instantiated: ", e);
		}
	}
}
