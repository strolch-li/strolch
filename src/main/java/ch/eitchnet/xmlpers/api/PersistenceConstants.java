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
package ch.eitchnet.xmlpers.api;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
@SuppressWarnings("nls")
public class PersistenceConstants {

	private static final String PROP_PREFIX = "ch.eitchnet.xmlpers.";
	public static final String PROP_VERBOSE = PROP_PREFIX + "verbose";
	public static final String PROP_BASEPATH = PROP_PREFIX + "basePath";
	public static final String PROP_DAO_FACTORY_CLASS = PROP_PREFIX + "daoFactoryClass";
	public static final String PROP_XML_IO_MOD = PROP_PREFIX + "ioMode";
	public static final String PROP_XML_LOCK_TIME_MILLIS = PROP_PREFIX + "lockTimeSeconds";
}
