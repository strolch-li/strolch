/*
 * Copyright (c) 2010
 * 
 * Robert von Burg
 * eitch@eitchnet.ch
 * 
 * All rights reserved.
 * 
 */

package ch.eitchnet.privilege.handler;

import java.util.Map;

/**
 * The {@link EncryptionHandler} exposes API which is used to handle encrypting of strings, or returning secure tokens
 * for certificates and so forth
 * 
 * @author rvonburg
 * 
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
	 * Initialize the concrete {@link EncryptionHandler}. The passed parameter map contains any configuration the
	 * concrete {@link EncryptionHandler} might need
	 * 
	 * @param parameterMap
	 *            a map containing configuration properties
	 */
	public void initialize(Map<String, String> parameterMap);
}
