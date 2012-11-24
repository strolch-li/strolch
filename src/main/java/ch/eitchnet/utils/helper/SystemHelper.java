/*
 * Copyright (c) 2012
 * 
 * This file is part of ch.eitchnet.java.utils
 *
 * ch.eitchnet.java.utils is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * ch.eitchnet.java.utils is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with ch.eitchnet.java.utils.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */
package ch.eitchnet.utils.helper;

/**
 * A helper class for {@link System} methods
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class SystemHelper {

	private static final SystemHelper instance;

	static {
		instance = new SystemHelper();
	}

	public static SystemHelper getInstance() {
		return SystemHelper.instance;
	}

	public static final String osName = System.getProperty("os.name");
	public static final String osArch = System.getProperty("os.arch");
	public static final String osVersion = System.getProperty("os.version");
	public static final String javaVendor = System.getProperty("java.vendor");
	public static final String javaVersion = System.getProperty("java.version");

	/**
	 * private constructor
	 */
	private SystemHelper() {
		//
	}

	/**
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return SystemHelper.asString();
	}

	/**
	 * @see java.lang.Object#toString()
	 */
	public static String asString() {
		StringBuilder sb = new StringBuilder();
		sb.append(SystemHelper.osName);
		sb.append(" ");
		sb.append(SystemHelper.osArch);
		sb.append(" ");
		sb.append(SystemHelper.osVersion);
		sb.append(" ");
		sb.append("on Java " + SystemHelper.javaVendor);
		sb.append(" version ");
		sb.append(SystemHelper.javaVersion);
		return sb.toString();
	}

	public static String getUserDir() {
		return System.getProperty("user.dir");
	}

	public static boolean isMacOS() {
		return SystemHelper.osName.startsWith("Mac");
	}

	public static boolean isWindows() {
		return SystemHelper.osName.startsWith("Win");
	}

	public static boolean isLinux() {
		return SystemHelper.osName.startsWith("Lin");
	}

	public static boolean is32bit() {
		return SystemHelper.osArch.equals("x86") || SystemHelper.osArch.equals("i386")
				|| SystemHelper.osArch.equals("i686");
	}

	public static boolean is64bit() {
		return SystemHelper.osArch.equals("x86_64") || SystemHelper.osArch.equals("amd64");
	}

	public static String getMaxMemory() {
		return FileHelper.humanizeFileSize(Runtime.getRuntime().maxMemory());
	}

	public static String getUsedMemory() {
		return FileHelper.humanizeFileSize(Runtime.getRuntime().totalMemory());
	}

	public static String getFreeMemory() {
		return FileHelper.humanizeFileSize(Runtime.getRuntime().freeMemory());
	}

	public static String getMemorySummary() {
		return "Memory available " + SystemHelper.getMaxMemory() + " / Used: " + SystemHelper.getUsedMemory()
				+ " / Free:" + SystemHelper.getFreeMemory();
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
		return Boolean.valueOf(SystemHelper.getProperty(context, key, def == null ? null : def.toString()));
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
		return Integer.valueOf(SystemHelper.getProperty(context, key, def == null ? null : def.toString()));
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
		return Double.valueOf(SystemHelper.getProperty(context, key, def == null ? null : def.toString()));
	}
}
