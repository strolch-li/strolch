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
 * @author rvonburg
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
