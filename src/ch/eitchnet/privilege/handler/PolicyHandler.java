/*
 * Copyright (c) 2010
 * 
 * Robert von Burg
 * eitch@eitchnet.ch
 * 
 * All rights reserved.
 * 
 */

package ch.eitchnet.privilege.handler;

import ch.eitchnet.privilege.base.PrivilegeContainerObject;
import ch.eitchnet.privilege.model.Restrictable;
import ch.eitchnet.privilege.model.internal.Role;

/**
 * @author rvonburg
 * 
 */
public interface PolicyHandler extends PrivilegeContainerObject {

	public boolean actionAllowed(Role role, Restrictable restrictable);
}
