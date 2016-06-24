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
	public static final String PROP_LOCK_TIME_MILLIS = PROP_PREFIX + "lockTimeSeconds";
}
