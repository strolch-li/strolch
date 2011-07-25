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
 * Exception thrown if access is denied during login, or if a certain privilege is not granted
 * 
 * @author rvonburg
 */
public class AccessDeniedException extends PrivilegeException {

	private static final long serialVersionUID = 1L;

	/**
	 * @param msg
	 *            detail on why and where access was denied
	 */
	public AccessDeniedException(String msg) {
		super(msg);
	}
}
