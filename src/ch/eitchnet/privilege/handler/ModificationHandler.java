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
import ch.eitchnet.privilege.model.Certificate;
import ch.eitchnet.privilege.model.PrivilegeRep;
import ch.eitchnet.privilege.model.RoleRep;
import ch.eitchnet.privilege.model.UserRep;
import ch.eitchnet.privilege.model.UserState;
import ch.eitchnet.privilege.model.internal.Privilege;

/**
 * @author rvonburg
 * 
 */
public interface ModificationHandler extends PrivilegeContainerObject {

	public void addOrReplaceUser(Certificate certificate, UserRep userRep);

	public void setUserPassword(Certificate certificate, String username, String password);

	public void setUserState(Certificate certificate, String username, UserState state);

	public void addOrReplaceRole(Certificate certificate, RoleRep roleRep);

	public void addRoleToUser(Certificate certificate, String username, String role);

	public void addOrReplacePrivilege(Certificate certificate, Privilege privilege);

	public void addPrivilegeToRole(Certificate certificate, String roleName, PrivilegeRep privilegeRep);

}
