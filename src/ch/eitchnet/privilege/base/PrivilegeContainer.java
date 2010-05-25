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
import ch.eitchnet.privilege.handler.SessionHandler;
import ch.eitchnet.privilege.handler.EncryptionHandler;

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

	private SessionHandler privilegeHandler;
	private PolicyHandler policyHandler;
	private EncryptionHandler encryptionHandler;

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
	public SessionHandler getPrivilegeHandler() {
		return privilegeHandler;
	}

	/**
	 * @return the policyHandler
	 */
	public PolicyHandler getPolicyHandler() {
		return policyHandler;
	}

	/**
	 * @return the encryptionHandler
	 */
	public EncryptionHandler getEncryptionHandler() {
		return encryptionHandler;
	}

	public void initialize(File privilegeContainerXml) {
		// TODO implement
	}

}
