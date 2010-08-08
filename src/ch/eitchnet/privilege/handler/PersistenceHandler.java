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

import org.dom4j.Element;

import ch.eitchnet.privilege.model.Certificate;
import ch.eitchnet.privilege.model.internal.Privilege;
import ch.eitchnet.privilege.model.internal.Role;
import ch.eitchnet.privilege.model.internal.User;
import ch.eitchnet.privilege.policy.PrivilegePolicy;

/**
 * @author rvonburg
 * 
 */
public interface PersistenceHandler {

	public User getUser(String username);

	public void addOrReplaceUser(User user);

	public User removeUser(String username);

	public Role getRole(String roleName);

	public void addOrReplaceRole(Role role);

	public Role removeRole(String roleName);

	public Privilege getPrivilege(String privilegeName);

	public void addOrReplacePrivilege(Privilege privilege);

	public Privilege removePrivilege(String privilegeName);

	public PrivilegePolicy getPolicy(String policyName);

	public boolean persist(Certificate certificate);

	public void initialize(Element element);
}
