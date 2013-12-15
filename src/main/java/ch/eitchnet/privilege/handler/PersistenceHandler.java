/*
 * Copyright 2013 Robert von Burg <eitch@eitchnet.ch>
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *     http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package ch.eitchnet.privilege.handler;

import java.util.List;
import java.util.Map;

import ch.eitchnet.privilege.model.IPrivilege;
import ch.eitchnet.privilege.model.Restrictable;
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
 * and {@link IPrivilege}
 * </p>
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public interface PersistenceHandler {

	/**
	 * Returns all currently known {@link User}s
	 * 
	 * @return all currently known {@link User}s
	 */
	public List<User> getAllUsers();

	/**
	 * Returns all currently known {@link Role}s
	 * 
	 * @return all currently known {@link Role}s
	 */
	public List<Role> getAllRoles();

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
	 * Informs this {@link PersistenceHandler} to persist any changes which need to be saved
	 * 
	 * @return true if changes were persisted successfully, false if nothing needed to be persisted
	 */
	public boolean persist();

	/**
	 * Informs this {@link PersistenceHandler} to reload the data from the backend
	 * 
	 * @return true if the reload was successful, false if something went wrong
	 */
	public boolean reload();

	/**
	 * Initialize the concrete {@link PersistenceHandler}. The passed parameter map contains any configuration the
	 * concrete {@link PersistenceHandler} might need
	 * 
	 * @param parameterMap
	 *            a map containing configuration properties
	 */
	public void initialize(Map<String, String> parameterMap);
}
