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

	public void addOrReplaceUser(User user);

	public User removeUser(String username);

	public Role getRole(String roleName);

	public void addOrReplaceRole(Role role);

	public Role removeRole(String roleName);

	public Privilege getPrivilege(String privilegeName);

	public void addOrReplacePrivilege(Privilege privilege);

	public Privilege removePrivilege(String privilegeName);

	public boolean persist(Certificate certificate);
}
