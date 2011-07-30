/*
 * Copyright (c) 2010
 * 
 * Robert von Burg
 * eitch@eitchnet.ch
 * 
 * All rights reserved.
 * 
 */

package ch.eitchnet.privilege.policy;

import ch.eitchnet.privilege.i18n.AccessDeniedException;
import ch.eitchnet.privilege.model.Restrictable;
import ch.eitchnet.privilege.model.internal.Privilege;
import ch.eitchnet.privilege.model.internal.Role;
import ch.eitchnet.privilege.model.internal.User;

/**
 * <p>
 * {@link PrivilegePolicy} implements logic to determine if a {@link User} which has the given {@link Role} and the
 * given {@link Privilege} has access to the given {@link Restrictable}
 * </p>
 * 
 * <p>
 * Re-think this interface and especially the {@link #actionAllowed(Role, Privilege, Restrictable)}-method... maybe we
 * need one with out the {@link Privilege} in its signature?
 * </p>
 * 
 * @author rvonburg
 */
public interface PrivilegePolicy {

	/**
	 * Checks if the given {@link Role} and the given {@link Privilege} has access to the given {@link Restrictable}
	 * 
	 * @param role
	 *            the {@link Role} trying to access the {@link Restrictable}
	 * @param privilege
	 *            the {@link Privilege} to check with
	 * @param restrictable
	 *            the {@link Restrictable} to which the user wants access
	 * 
	 * @throws AccessDeniedException
	 *             if action not allowed
	 */
	public void actionAllowed(Role role, Privilege privilege, Restrictable restrictable) throws AccessDeniedException;
}
