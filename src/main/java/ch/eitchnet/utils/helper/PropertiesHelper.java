/*
 * Copyright (c) 2012, Robert von Burg
 *
 * All rights reserved.
 *
 * This file is part of the XXX.
 *
 *  XXX is free software: you can redistribute 
 *  it and/or modify it under the terms of the GNU General Public License as 
 *  published by the Free Software Foundation, either version 3 of the License, 
 *  or (at your option) any later version.
 *
 *  XXX is distributed in the hope that it will 
 *  be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with XXX.  If not, see 
 *  <http://www.gnu.org/licenses/>.
 */
package ch.eitchnet.utils.helper;

import java.util.Properties;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public class PropertiesHelper {

	/**
	 * Returns the property with the given key from the given {@link Properties}. If def is null, and the property is
	 * not set, then a {@link RuntimeException} is thrown
	 * 
	 * @param properties
	 *            the {@link Properties} from which to retrieve the property
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
	public static String getProperty(Properties properties, String context, String key, String def)
			throws RuntimeException {
		String property = properties.getProperty(key, def);
		if (property == null)
			throw new RuntimeException("[" + context + "] Property " + key + " is not set, and no default was given!");

		return property;
	}

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
		return getProperty(System.getProperties(), context, key, def);
	}

	/**
	 * Delegates to {@link #getProperty(Properties, String, String, String)} but returns the value as a {@link Boolean}
	 * where {@link Boolean#valueOf(String)} defines the actual value
	 * 
	 * @param properties
	 *            the {@link Properties} from which to retrieve the property
	 * @return the property as a {@link Boolean} under the given key
	 * 
	 * @throws RuntimeException
	 *             if the property is not set and def is null
	 */
	public static Boolean getPropertyBool(Properties properties, String context, String key, Boolean def)
			throws RuntimeException {
		return Boolean.valueOf(getProperty(properties, context, key, def == null ? null : def.toString()));
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
	 * Delegates to {@link #getProperty(Properties, String, String, String)} but returns the value as an {@link Integer}
	 * where {@link Integer#valueOf(String)} defines the actual value
	 * 
	 * @return the property as a {@link Integer} under the given key
	 * 
	 * @throws RuntimeException
	 *             if the property is not set and def is null
	 */
	public static Integer getPropertyInt(Properties properties, String context, String key, Integer def)
			throws RuntimeException {
		return Integer.valueOf(getProperty(properties, context, key, def == null ? null : def.toString()));
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
	 * Delegates to {@link #getProperty(Properties, String, String, String)} but returns the value as an {@link Double}
	 * where {@link Double#valueOf(String)} defines the actual value
	 * 
	 * @return the property as a {@link Double} under the given key
	 * 
	 * @throws RuntimeException
	 *             if the property is not set and def is null
	 */
	public static Double getPropertyDouble(Properties properties, String context, String key, Double def)
			throws RuntimeException {
		return Double.valueOf(getProperty(properties, context, key, def == null ? null : def.toString()));
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

	/**
	 * Delegates to {@link #getProperty(Properties, String, String, String)} but returns the value as an {@link Long}
	 * where {@link Long#valueOf(String)} defines the actual value
	 * 
	 * @return the property as a {@link Long} under the given key
	 * 
	 * @throws RuntimeException
	 *             if the property is not set and def is null
	 */
	public static Long getPropertyLong(Properties properties, String context, String key, Long def)
			throws RuntimeException {
		return Long.valueOf(getProperty(properties, context, key, def == null ? null : def.toString()));
	}

	/**
	 * Delegates to {@link #getProperty(String, String, String)} but returns the value as an {@link Long} where
	 * {@link Long#valueOf(String)} defines the actual value
	 * 
	 * @return the property as a {@link Long} under the given key
	 * 
	 * @throws RuntimeException
	 *             if the property is not set and def is null
	 */
	public static Long getPropertyLong(String context, String key, Long def) throws RuntimeException {
		return Long.valueOf(getProperty(context, key, def == null ? null : def.toString()));
	}
}
