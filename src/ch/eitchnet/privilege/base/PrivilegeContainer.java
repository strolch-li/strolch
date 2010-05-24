/*
 * Copyright (c) 2010
 * 
 * Robert von Burg
 * eitch@eitchnet.ch
 * 
 * All rights reserved.
 * 
 */

package ch.eitchnet.privilege.base;

import java.io.File;

import ch.eitchnet.privilege.handler.PolicyHandler;
import ch.eitchnet.privilege.handler.PrivilegeHandler;

/**
 * 
 * 
 * @author rvonburg
 */
public class PrivilegeContainer {

	private static final PrivilegeContainer instance;

	static {
		instance = new PrivilegeContainer();
	}

	private PrivilegeHandler privilegeHandler;
	private PolicyHandler policyHandler;

	public static PrivilegeContainer getInstance() {
		return instance;
	}

	/**
	 * private constructor to force singleton
	 */
	private PrivilegeContainer() {
		// private constructor
	}

	/**
	 * @return the privilegeHandler
	 */
	public PrivilegeHandler getPrivilegeHandler() {
		return privilegeHandler;
	}

	/**
	 * @return the policyHandler
	 */
	public PolicyHandler getPolicyHandler() {
		return policyHandler;
	}

	public void initialize(File privilegeContainerXml) {
		// TODO implement
	}

}
