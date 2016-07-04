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
import java.util.Properties;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
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
		if (property == null) {
			String msg = "[{0}] Property {1} is not set, and no default was given!"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, context, key);
			throw new RuntimeException(msg);
		}

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
