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
public class AccessDeniedException extends PrivilegeException {

	private static final long serialVersionUID = 1L;

	/**
	 * @param string
	 */
	public AccessDeniedException(String string) {
		super(string);
	}
}
