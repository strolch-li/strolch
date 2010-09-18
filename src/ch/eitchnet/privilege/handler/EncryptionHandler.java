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
 * @author rvonburg
 * 
 */
public interface EncryptionHandler {

	/**
	 * @return
	 */
	public String nextToken();

	/**
	 * @param string
	 * @return
	 */
	public String convertToHash(String string);

	/**
	 * @param parameterMap
	 */
	public void initialize(Map<String, String> parameterMap);
}
