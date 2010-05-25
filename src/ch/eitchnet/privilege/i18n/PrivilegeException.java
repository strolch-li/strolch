/*
 * Copyright (c) 2010
 * 
 * Robert von Burg
 * eitch@eitchnet.ch
 * 
 * All rights reserved.
 * 
 */

package ch.eitchnet.privilege.i18n;

/**
 * @author rvonburg
 * 
 */
public class PrivilegeException extends RuntimeException {

	private static final long serialVersionUID = 1L;

	/**
	 * @param string
	 */
	public PrivilegeException(String string) {
		super(string);
	}

	/**
	 * 
	 * @param string
	 * @param t
	 */
	public PrivilegeException(String string, Throwable t) {
		super(string, t);
	}
}
