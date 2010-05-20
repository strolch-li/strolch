/*
 * Copyright (c) 2010
 * 
 * Robert von Burg
 * eitch@eitchnet.ch
 * 
 * All rights reserved.
 * 
 */

package ch.eitchnet.privilege.base;

/**
 * @author rvonburg
 * 
 */
public class PrivilegeException extends RuntimeException {

	/**
	 * @param string
	 */
	public PrivilegeException(String string) {
		super(string);
	}

	private static final long serialVersionUID = 1L;

}
