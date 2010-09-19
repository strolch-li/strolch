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
import ch.eitchnet.privilege.model.Restrictable;
import ch.eitchnet.privilege.model.internal.Privilege;
import ch.eitchnet.privilege.model.internal.Role;
import ch.eitchnet.privilege.model.internal.User;
import ch.eitchnet.privilege.policy.PrivilegePolicy;

/**
 * <p>
 * The {@link PersistenceHandler} takes care of retrieving and persisting model objects to the underlying database. This
 * database can be simple XML files, or an LDAP and so forth
 * </p>
 * 
 * <p>
 * The {@link PersistenceHandler} also serves the special {@link PrivilegePolicy} objects. These policies are special
 * objects which implement an algorithm to define if an action is allowed on a {@link Restrictable} by a {@link Role}
 * and {@link Privilege}
 * </p>
 * 
 * @author rvonburg
 * 
 */
public interface PersistenceHandler {

	/**
	 * Returns a {@link User} object from the underlying database
	 * 
	 * @param username
	 *            the name/id of the {@link User} object to return
	 * 
	 * @return the {@link User} object, or null if it was not found
	 */
	public User getUser(String username);

	/**
	 * Returns a {@link Role} object from the underlying database
	 * 
	 * @param roleName
	 *            the name/id of the {@link Role} object to return
	 * 
	 * @return the {@link Role} object, or null if it was not found
	 */
	public Role getRole(String roleName);

	/**
	 * Returns a {@link Privilege} object from the underlying database
	 * 
	 * @param privilegeName
	 *            the name/id of the {@link Privilege} object to return
	 * 
	 * @return the {@link Privilege} object, or null if it was not found
	 */
	public Privilege getPrivilege(String privilegeName);

	/**
	 * <p>
	 * Thus this method instantiates a {@link PrivilegePolicy} object from the given policyName. The
	 * {@link PrivilegePolicy} is not stored in a database, but rather behind a privilege name a class name is stored
	 * which then is used to instantiate a new object
	 * </p>
	 * 
	 * @param policyName
	 *            the name/id of the {@link PrivilegePolicy} object to return
	 * 
	 * @return the {@link PrivilegePolicy} object, or null if no class is defined for the given policy name
	 */
	public PrivilegePolicy getPolicy(String policyName);

	/**
	 * Removes a {@link User} with the given name and returns the removed object if it existed
	 * 
	 * @param username
	 *            the name of the {@link User} to remove
	 * 
	 * @return the {@link User} removed, or null if it did not exist
	 */
	public User removeUser(String username);

	/**
	 * Removes a {@link Role} with the given name and returns the removed object if it existed
	 * 
	 * @param roleName
	 *            the name of the {@link Role} to remove
	 * 
	 * @return the {@link Role} removed, or null if it did not exist
	 */
	public Role removeRole(String roleName);

	/**
	 * Removes a {@link Privilege} with the given name and returns the removed object if it existed
	 * 
	 * @param privilegeName
	 *            the name of the {@link Privilege} to remove
	 * 
	 * @return the {@link Privilege} removed, or null if it did not exist
	 */
	public Privilege removePrivilege(String privilegeName);

	/**
	 * Adds a {@link User} object to the underlying database. If the {@link User} already exists, it is replaced
	 * 
	 * @param user
	 *            the {@link User} object to add
	 */
	public void addOrReplaceUser(User user);

	/**
	 * Adds a {@link Role} object to the underlying database. If the {@link Role} already exists, it is replaced
	 * 
	 * @param role
	 *            the {@link User} object to add
	 */
	public void addOrReplaceRole(Role role);

	/**
	 * Adds a {@link Privilege} object to the underlying database. If the {@link Privilege} already exists, it is
	 * replaced
	 * 
	 * @param privilege
	 *            the {@link Privilege} object to add
	 */
	public void addOrReplacePrivilege(Privilege privilege);

	/**
	 * @param certificate
	 * 
	 * @return
	 */
	public boolean persist(Certificate certificate);

	/**
	 * @param parameterMap
	 */
	public void initialize(Map<String, String> parameterMap);
}
