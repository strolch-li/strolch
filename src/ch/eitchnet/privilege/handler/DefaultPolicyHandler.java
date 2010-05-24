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

import java.io.File;
import java.util.Map;

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
		
		// TODO auth user
		
		// TODO Auto-generated method stub
		return false;
	}

	public void initialize(File policyXml) {
		// TODO implement
	}
}
