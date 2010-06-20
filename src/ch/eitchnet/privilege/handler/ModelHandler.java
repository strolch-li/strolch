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

import java.util.List;
import java.util.Locale;

import ch.eitchnet.privilege.base.PrivilegeContainerObject;
import ch.eitchnet.privilege.model.Certificate;
import ch.eitchnet.privilege.model.PrivilegeRep;
import ch.eitchnet.privilege.model.RoleRep;
import ch.eitchnet.privilege.model.UserRep;
import ch.eitchnet.privilege.model.UserState;
import ch.eitchnet.privilege.model.internal.Privilege;
import ch.eitchnet.privilege.model.internal.Role;
import ch.eitchnet.privilege.model.internal.User;

/**
 * @author rvonburg
 * 
 */
public interface ModelHandler extends PrivilegeContainerObject {

	public void setPersistenceHandler(PersistenceHandler persistenceHandler);

	public User getUser(String username);

	public void addOrReplaceUser(Certificate certificate, UserRep userRep);

	public UserRep removeUser(Certificate certificate, String username);

	public void setUserPassword(Certificate certificate, String username, String password);

	public void setUserNamePassword(Certificate certificate, String username, String firstname, String surname);

	public void setUserState(Certificate certificate, String username, UserState state);

	public void setUserLocaleState(Certificate certificate, String username, Locale locale);

	public void addRoleToUser(Certificate certificate, String username, String rolename);

	public void removeRoleFromUser(Certificate certificate, String username, String rolename);

	public void addOrReplaceRole(Certificate certificate, RoleRep roleRep);

	public Role getRole(String roleName);

	public RoleRep removeRole(Certificate certificate, String roleName);

	public void addPrivilegeToRole(Certificate certificate, String roleName, String privilegeName);

	public void removePrivilegeFromRole(Certificate certificate, String roleName, String privilegeName);

	public Privilege getPrivilege(String privilegeName);

	public void addOrReplacePrivilege(Certificate certificate, PrivilegeRep privilegeRep);

	public PrivilegeRep removePrivilege(Certificate certificate, String privilegeName);

	public void setPrivilegePolicy(Certificate certificate, String privilegeName, String policyName);

	public void setPrivilegeAllAllowed(Certificate certificate, String privilegeName, boolean allAllowed);

	public void setPrivilegeDenyList(Certificate certificate, String privilegeName, List<String> denyList);

	public void setPrivilegeAllowList(Certificate certificate, String privilegeName, List<String> allowList);

	public boolean persist(Certificate certificate);
}
