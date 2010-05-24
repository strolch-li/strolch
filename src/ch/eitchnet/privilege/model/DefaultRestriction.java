/*
 * Copyright (c) 2010
 * 
 * Robert von Burg
 * eitch@eitchnet.ch
 * 
 * All rights reserved.
 * 
 */

package ch.eitchnet.privilege.model;

import java.util.List;
import java.util.Map;

import org.dom4j.Element;

import ch.eitchnet.privilege.i18n.PrivilegeException;

/**
 * @author rvonburg
 * 
 */
public class DefaultRestriction implements RestrictionPolicy {

	private String restrictionKey;

	private Map<String, Restriction> roleRestrictionMap;

	/**
	 * @see ch.eitchnet.privilege.model.RestrictionPolicy#actionAllowed(ch.eitchnet.privilege.model.User,
	 *      ch.eitchnet.privilege.model.Restrictable)
	 */
	@Override
	public boolean actionAllowed(User user, Restrictable restrictable) {

		// validate user is not null
		if (user == null)
			throw new PrivilegeException("User may not be null!");

		// validate Restrictable is set for this RestrictionPolicy
		if (!restrictionKey.equals(restrictable.getRestrictionKey())) {
			throw new PrivilegeException(RestrictionPolicy.class.getSimpleName() + " "
					+ DefaultRestriction.class.getSimpleName() + " with restriction key " + restrictionKey
					+ " can not validate " + Restrictable.class.getSimpleName() + " with key "
					+ restrictable.getRestrictionKey());
		}

		// default is that user does not have privilege
		boolean hasPrivilege = false;

		// iterate user roles and validate role has privilege
		for (String role : user.getRoleList()) {

			hasPrivilege = internalActionAllowed(role, restrictable);

			// if privilege is found, then stop iterating
			if (hasPrivilege)
				break;
		}

		return hasPrivilege;
	}

	/**
	 * @param role
	 * @param restrictable
	 * @return
	 */
	private boolean internalActionAllowed(String role, Restrictable restrictable) {

		// get restriction object for users role
		Restriction restriction = roleRestrictionMap.get(role);

		// no restriction object means no privilege
		if (restriction == null)
			return false;

		// does this role have privilege for any values?
		if (restriction.anyValue)
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
		for (String notAllowed : restriction.valuesNotAllowed) {
			if (notAllowed.equals(restrictionValue))
				return false;
		}

		// now check values allowed
		for (String allowed : restriction.valuesAllowed) {
			if (allowed.equals(restrictionValue))
				return true;
		}

		// default is not allowed
		return false;
	}

	public void initialize(Element element) {

		// TODO implement
	}

	private class Restriction {
		private boolean anyValue;
		private List<String> valuesAllowed;
		private List<String> valuesNotAllowed;

		/**
		 * @param allAllowed
		 * @param valuesAllowed
		 * @param valuesNotAllowed
		 */
		public Restriction(boolean anyValue, List<String> valuesAllowed, List<String> valuesNotAllowed) {
			this.anyValue = anyValue;
			this.valuesAllowed = valuesAllowed;
			this.valuesNotAllowed = valuesNotAllowed;
		}
	}
}
