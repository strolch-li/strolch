/*
 * Copyright (c) 2010, 2011
 * 
 * Robert von Burg <eitch@eitchnet.ch>
 * 
 */

/*
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

package ch.eitchnet.privilege.i18n;

/**
 * Main {@link RuntimeException} thrown if something goes wrong in Privilege
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
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
