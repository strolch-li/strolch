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

import ch.eitchnet.privilege.model.internal.User;
import ch.eitchnet.privilege.policy.RestrictionPolicy;

/**
 * @author rvonburg
 * 
 */
public interface PersistenceHandler {

	public List<User> getAllUsers();

	public void saveUsers(List<User> users);

	public List<RestrictionPolicy> getAllRestrictionPolicies();
}
