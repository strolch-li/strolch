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
package ch.eitchnet.privilege.handler;

import java.util.Map;

/**
 * The {@link EncryptionHandler} exposes API which is used to handle encrypting of strings, or returning secure tokens
 * for certificates and so forth
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public interface EncryptionHandler {

	/**
	 * Calculates or generates a token which can be used to identify certificates and so forth
	 * 
	 * @return the secure token
	 */
	public String nextToken();

	/**
	 * Converts a given string, e.g. a password to a hash which is defined by the concrete implementation
	 * 
	 * @param string
	 *            the string to convert
	 * @return the hash of the string after converting
	 */
	public String convertToHash(String string);
	
	/**
	 * Converts a given byte array, e.g. a password to a hash which is defined by the concrete implementation
	 * 
	 * @param bytes
	 *            the bytes to convert
	 * @return the hash of the string after converting
	 */
	public String convertToHash(byte[] bytes);

	/**
	 * Initialize the concrete {@link EncryptionHandler}. The passed parameter map contains any configuration the
	 * concrete {@link EncryptionHandler} might need
	 * 
	 * @param parameterMap
	 *            a map containing configuration properties
	 */
	public void initialize(Map<String, String> parameterMap);
}
