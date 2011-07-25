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
 * Main {@link RuntimeException} thrown if something goes wrong in Privilege
 * 
 * @author rvonburg
 */
public class PrivilegeException extends RuntimeException {

	private static final long serialVersionUID = 1L;

	/**
	 * Default constructor
	 * 
	 * @param string
	 *            message to go with the exception
	 */
	public PrivilegeException(String string) {
		super(string);
	}

	/**
	 * Constructor with underlying exception
	 * 
	 * @param string
	 *            message to go with the exception
	 * @param t
	 *            throwable to wrap with this exception which is the underlying exception of this exception
	 */
	public PrivilegeException(String string, Throwable t) {
		super(string, t);
	}
}
