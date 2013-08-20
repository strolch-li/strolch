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
}
