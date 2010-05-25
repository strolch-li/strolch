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

import org.dom4j.Element;

import ch.eitchnet.privilege.i18n.PrivilegeException;
import ch.eitchnet.privilege.model.Restrictable;
import ch.eitchnet.privilege.model.RestrictionPolicy;
import ch.eitchnet.privilege.model.User;

/**
 * @author rvonburg
 * 
 */
public class DefaultPolicyHandler implements PolicyHandler {

	private Map<String, RestrictionPolicy> policyMap;

	/**
	 * @see ch.eitchnet.privilege.handler.PolicyHandler#actionAllowed(ch.eitchnet.privilege.model.User,
	 *      ch.eitchnet.privilege.model.Restrictable)
	 */
	@Override
	public boolean actionAllowed(User user, Restrictable restrictable) {

		// user and restrictable must not be null
		if (user == null)
			throw new PrivilegeException("User may not be null!");
		else if (restrictable == null)
			throw new PrivilegeException("Restrictable may not be null!");

		// validate restriction key for this restrictable
		String restrictionKey = restrictable.getRestrictionKey();
		if (restrictionKey == null || restrictionKey.length() < 3) {
			throw new PrivilegeException(
					"The RestrictionKey may not be shorter than 3 characters. Invalid Restrictable "
							+ restrictable.getClass().getName());
		}

		// get restriction policy
		RestrictionPolicy policy = policyMap.get(restrictionKey);
		if (policy == null) {
			throw new PrivilegeException("No RestrictionPolicy exists for the RestrictionKey " + restrictionKey
					+ " for Restrictable " + restrictable.getClass().getName());
		}

		// delegate checking to restriction policy
		return policy.actionAllowed(user, restrictable);
	}

	/**
	 * @see ch.eitchnet.privilege.base.PrivilegeContainerObject#initialize(org.dom4j.Element)
	 */
	public void initialize(Element element) {
		// TODO implement
	}
}
