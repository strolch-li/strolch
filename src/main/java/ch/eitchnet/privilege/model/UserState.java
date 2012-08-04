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
package ch.eitchnet.privilege.model;

import ch.eitchnet.privilege.model.internal.User;

/**
 * The {@link UserState} enum defines the different states a {@link User} can have:
 * <ul>
 * <li>NEW - the user is new, and cannot login</li>
 * <li>ENABLED - the user has been enabled, meaning a password has been set and the user has at least one role assigned
 * and may thus login</li>
 * <li>DISABLED - the user been disabled by an administrator</li>
 * <li>EXPIRED - the user has automatically expired through a predefined time</li>
 * </ul>
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public enum UserState {
	/**
	 * the user is new, and cannot login
	 */
	NEW,
	/**
	 * the user has been enabled, meaning a password has been set and the user has at least one role assigned and may
	 * thus login
	 */
	ENABLED,
	/**
	 * the user been disabled by an administrator
	 */
	DISABLED,
	/**
	 * the user has automatically expired through a predefined time
	 */
	EXPIRED,
	
	/**
	 * This is the System user state which is special and thus exempted from normal uses
	 */
	SYSTEM;
}
