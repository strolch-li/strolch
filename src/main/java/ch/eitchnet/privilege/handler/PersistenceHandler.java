/*
 * Copyright (c) 2010 - 2012
 * 
 * This file is part of Privilege.
 *
 * Privilege is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Privilege is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with Privilege.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */
package ch.eitchnet.privilege.handler;

import java.util.List;
import java.util.Map;

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
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
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
