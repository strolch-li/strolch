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

/**
 * TODO javadoc
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
