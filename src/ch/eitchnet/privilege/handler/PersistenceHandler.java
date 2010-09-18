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

import java.util.Map;

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

	/**
	 * @param username
	 * @return
	 */
	public User getUser(String username);

	/**
	 * @param user
	 */
	public void addOrReplaceUser(User user);

	/**
	 * @param username
	 * @return
	 */
	public User removeUser(String username);

	/**
	 * @param roleName
	 * @return
	 */
	public Role getRole(String roleName);

	/**
	 * @param role
	 */
	public void addOrReplaceRole(Role role);

	/**
	 * @param roleName
	 * @return
	 */
	public Role removeRole(String roleName);

	/**
	 * @param privilegeName
	 * @return
	 */
	public Privilege getPrivilege(String privilegeName);

	/**
	 * @param privilege
	 */
	public void addOrReplacePrivilege(Privilege privilege);

	/**
	 * @param privilegeName
	 * @return
	 */
	public Privilege removePrivilege(String privilegeName);

	/**
	 * @param policyName
	 * @return
	 */
	public PrivilegePolicy getPolicy(String policyName);

	/**
	 * @param certificate
	 * @return
	 */
	public boolean persist(Certificate certificate);
	
	/**
	 * @param parameterMap
	 */
	public void initialize(Map<String, String> parameterMap);
}
