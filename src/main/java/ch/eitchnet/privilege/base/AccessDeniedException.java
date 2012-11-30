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
package ch.eitchnet.privilege.base;

/**
 * Exception thrown if access is denied during login, or if a certain privilege is not granted
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
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
