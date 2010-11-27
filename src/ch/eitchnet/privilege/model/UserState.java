/*
 * Copyright (c) 2010
 * 
 * Robert von Burg
 * eitch@eitchnet.ch
 * 
 * All rights reserved.
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
 * @author rvonburg
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
	EXPIRED;
}
