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

import ch.eitchnet.privilege.base.PrivilegeContainer;
import ch.eitchnet.privilege.base.PrivilegeContainerObject;
import ch.eitchnet.privilege.model.Certificate;
import ch.eitchnet.privilege.model.internal.Privilege;
import ch.eitchnet.privilege.model.internal.Role;
import ch.eitchnet.privilege.model.internal.User;

/**
 * TODO {@link PersistenceHandler} may not be freely accessible via {@link PrivilegeContainer}
 * 
 * @author rvonburg
 * 
 */
public interface PersistenceHandler extends PrivilegeContainerObject {

	public User getUser(String username);
	// public void setUserPassword(String username, String password);
	// public void setUserState(String username, UserState state);
	public void addUser(Certificate certificate, User user);

	public Role getRole(String roleName);

	public void addRole(Certificate certificate, Role role);

	public Privilege getPrivilege(String privilegeName);

	public void addPrivilege(Certificate certificate, Privilege privilege);

	public void persist(Certificate certificate);
}
