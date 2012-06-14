/*
 * Copyright (c) 2012
 * 
 * Robert von Burg <eitch@eitchnet.ch>
 * 
 */

/*
 * This file is part of ch.eitchnet.java.utils
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
package ch.eitchnet.utils.helper;

/**
 * A helper class for {@link System} methods
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class SystemHelper {

	/**
	 * Returns the {@link System#getProperty(String)} with the given key. If def is null, and the property is not set,
	 * then a {@link RuntimeException} is thrown
	 * 
	 * @param context
	 *            The context should be the name of the caller, so that stack trace gives enough detail as to which
	 *            class expected the configuration property if no property was found
	 * @param key
	 *            the key of the property to return
	 * @param def
	 *            the default value, if null, then an exception will be thrown if the property is not set
	 * 
	 * @return the property under the given key
	 * 
	 * @throws RuntimeException
	 *             if the property is not set and def is null
	 */
	public static String getProperty(String context, String key, String def) throws RuntimeException {
		String property = System.getProperty(key, def);
		if (property == null)
			throw new RuntimeException("[" + context + "] Property " + key + " is not set, and no default was given!");

		return property;
	}

	/**
	 * Delegates to {@link #getProperty(String, String, String)} but returns the value as a {@link Boolean} where
	 * {@link Boolean#valueOf(String)} defines the actual value
	 * 
	 * @return the property as a {@link Boolean} under the given key
	 * 
	 * @throws RuntimeException
	 *             if the property is not set and def is null
	 */
	public static Boolean getPropertyBool(String context, String key, Boolean def) throws RuntimeException {
		return Boolean.valueOf(getProperty(context, key, def == null ? null : def.toString()));
	}

	/**
	 * Delegates to {@link #getProperty(String, String, String)} but returns the value as an {@link Integer} where
	 * {@link Integer#valueOf(String)} defines the actual value
	 * 
	 * @return the property as a {@link Integer} under the given key
	 * 
	 * @throws RuntimeException
	 *             if the property is not set and def is null
	 */
	public static Integer getPropertyInt(String context, String key, Integer def) throws RuntimeException {
		return Integer.valueOf(getProperty(context, key, def == null ? null : def.toString()));
	}

	/**
	 * Delegates to {@link #getProperty(String, String, String)} but returns the value as an {@link Double} where
	 * {@link Double#valueOf(String)} defines the actual value
	 * 
	 * @return the property as a {@link Double} under the given key
	 * 
	 * @throws RuntimeException
	 *             if the property is not set and def is null
	 */
	public static Double getPropertyDouble(String context, String key, Double def) throws RuntimeException {
		return Double.valueOf(getProperty(context, key, def == null ? null : def.toString()));
	}
}
