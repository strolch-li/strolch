/*
 * Copyright 2013 Robert von Burg <eitch@eitchnet.ch>
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *     http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
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
