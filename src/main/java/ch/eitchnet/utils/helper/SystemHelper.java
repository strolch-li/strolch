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

	public static final String osName = System.getProperty("os.name"); //$NON-NLS-1$
	public static final String osArch = System.getProperty("os.arch"); //$NON-NLS-1$
	public static final String osVersion = System.getProperty("os.version"); //$NON-NLS-1$
	public static final String javaVendor = System.getProperty("java.vendor"); //$NON-NLS-1$
	public static final String javaVersion = System.getProperty("java.version"); //$NON-NLS-1$

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
		sb.append(StringHelper.EMPTY);
		sb.append(SystemHelper.osArch);
		sb.append(StringHelper.EMPTY);
		sb.append(SystemHelper.osVersion);
		sb.append(StringHelper.EMPTY);
		sb.append("on Java "); //$NON-NLS-1$
		sb.append(SystemHelper.javaVendor);
		sb.append(" version "); //$NON-NLS-1$
		sb.append(SystemHelper.javaVersion);
		return sb.toString();
	}

	public static String getUserDir() {
		return System.getProperty("user.dir"); //$NON-NLS-1$
	}

	public static boolean isMacOS() {
		return SystemHelper.osName.startsWith("Mac"); //$NON-NLS-1$
	}

	public static boolean isWindows() {
		return SystemHelper.osName.startsWith("Win"); //$NON-NLS-1$
	}

	public static boolean isLinux() {
		return SystemHelper.osName.startsWith("Lin"); //$NON-NLS-1$
	}

	public static boolean is32bit() {
		return SystemHelper.osArch.equals("x86") || SystemHelper.osArch.equals("i386") //$NON-NLS-1$ //$NON-NLS-2$
				|| SystemHelper.osArch.equals("i686"); //$NON-NLS-1$
	}

	public static boolean is64bit() {
		return SystemHelper.osArch.equals("x86_64") || SystemHelper.osArch.equals("amd64"); //$NON-NLS-1$ //$NON-NLS-2$
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
		StringBuilder sb = new StringBuilder();
		sb.append("Memory available "); //$NON-NLS-1$
		sb.append(SystemHelper.getMaxMemory());
		sb.append(" / Used: "); //$NON-NLS-1$
		sb.append(SystemHelper.getUsedMemory());
		sb.append(" / Free:"); //$NON-NLS-1$
		sb.append(SystemHelper.getFreeMemory());
		return sb.toString();
	}
}
