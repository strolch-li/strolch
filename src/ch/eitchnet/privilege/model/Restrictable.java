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

package ch.eitchnet.privilege.model;

import ch.eitchnet.privilege.model.internal.Privilege;
import ch.eitchnet.privilege.policy.PrivilegePolicy;

/**
 * <p>
 * Objects implementing this interface are used to grant/restrict privileges to them. A {@link PrivilegePolicy}
 * implements the logic on granting/restricting privileges for a {@link Restrictable} and the
 * {@link #getPrivilegeName()} is used to find the {@link Privilege} which has the associated {@link PrivilegePolicy}
 * for evaluating access
 * </p>
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public interface Restrictable {

	/**
	 * Returns the name of the {@link Privilege} which is to be used to validate privileges against
	 * 
	 * @return the name of the {@link Privilege} which is to be used to validate privileges against
	 */
	public String getPrivilegeName();

	/**
	 * Returns the value which defines or describes what privilege is to be granted
	 * 
	 * @return the value which defines or describes what privilege is to be granted
	 */
	public Object getPrivilegeValue();
}
