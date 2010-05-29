/*
 * Copyright (c) 2010
 * 
 * Robert von Burg
 * eitch@eitchnet.ch
 * 
 * All rights reserved.
 * 
 */

package ch.eitchnet.privilege.model.internal;

import org.dom4j.Element;

import ch.eitchnet.privilege.i18n.PrivilegeException;
import ch.eitchnet.privilege.model.Restrictable;

/**
 * @author rvonburg
 * 
 */
public class DefaultRestriction implements RestrictionPolicy {

	private String restrictionKey;

	/**
	 * @see ch.eitchnet.privilege.model.internal.RestrictionPolicy#actionAllowed(ch.eitchnet.privilege.model.User,
	 *      ch.eitchnet.privilege.model.Restrictable)
	 */
	@Override
	public boolean actionAllowed(Role role, Restrictable restrictable) {

		// validate user is not null
		if (role == null)
			throw new PrivilegeException("Role may not be null!");

		// validate Restrictable is set for this RestrictionPolicy
		if (!restrictionKey.equals(restrictable.getRestrictionKey())) {
			throw new PrivilegeException(RestrictionPolicy.class.getSimpleName() + " "
					+ DefaultRestriction.class.getSimpleName() + " with restriction key " + restrictionKey
					+ " can not validate " + Restrictable.class.getSimpleName() + " with key "
					+ restrictable.getRestrictionKey());
		}

		// get restriction object for users role
		Privilege privilege = role.getPrivilege(restrictionKey);

		// no restriction object means no privilege
		if (privilege == null)
			return false;

		// does this role have privilege for any values?
		if (privilege.isAllAllowed())
			return true;

		// get the value on which the action is to be performed
		Object object = restrictable.getRestrictionValue();

		// DefaultRestriction policy expects the restriction value to be a string
		if (!(object instanceof String)) {
			throw new PrivilegeException(Restrictable.class.getName() + " " + restrictable.getClass().getSimpleName()
					+ " has returned a non-string restriction value!");
		}

		String restrictionValue = (String) object;

		// first check values not allowed
		for (String notAllowed : privilege.getValuesNotAllowed()) {
			if (notAllowed.equals(restrictionValue))
				return false;
		}

		// now check values allowed
		for (String allowed : privilege.getValuesAllowed()) {
			if (allowed.equals(restrictionValue))
				return true;
		}

		// default is not allowed
		return false;
	}

	public void initialize(Element element) {

		// TODO implement
	}
}
