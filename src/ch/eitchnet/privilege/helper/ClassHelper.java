/*
 * Copyright (c) 2010
 * 
 * Robert von Burg
 * eitch@eitchnet.ch
 * 
 * All rights reserved.
 * 
 */

package ch.eitchnet.privilege.helper;

import ch.eitchnet.privilege.i18n.PrivilegeException;

/**
 * @author rvonburg
 * 
 */
public class ClassHelper {

	@SuppressWarnings("unchecked")
	public static <T> T instantiateClass(String className) {
		try {

			Class<T> clazz = (Class<T>) Class.forName(className);

			return clazz.getConstructor().newInstance();

		} catch (Exception e) {
			throw new PrivilegeException("The class " + className + " could not be instantiated: ", e);
		}
	}
}
