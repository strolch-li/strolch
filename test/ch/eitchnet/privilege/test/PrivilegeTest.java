/*
 * Copyright (c) 2010
 * 
 * Robert von Burg
 * eitch@eitchnet.ch
 * 
 * All rights reserved.
 * 
 */

package ch.eitchnet.privilege.test;

import java.io.File;
import java.util.HashSet;

import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.ConsoleAppender;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.apache.log4j.PatternLayout;
import org.junit.BeforeClass;
import org.junit.Test;

import ch.eitchnet.privilege.base.PrivilegeContainer;
import ch.eitchnet.privilege.handler.ModelHandler;
import ch.eitchnet.privilege.i18n.AccessDeniedException;
import ch.eitchnet.privilege.i18n.PrivilegeException;
import ch.eitchnet.privilege.model.Certificate;
import ch.eitchnet.privilege.model.UserRep;
import ch.eitchnet.privilege.model.UserState;

/**
 * @author rvonburg
 * 
 */
public class PrivilegeTest {

	private static final Logger logger = Logger.getLogger(PrivilegeTest.class);

	/**
	 * @throws java.lang.Exception
	 */
	@BeforeClass
	public static void init() throws Exception {

		// set up log4j
		BasicConfigurator.resetConfiguration();
		BasicConfigurator.configure(new ConsoleAppender(new PatternLayout("%d %5p [%t] %C{1} %M - %m%n")));
		Logger.getRootLogger().setLevel(Level.INFO);

		// initialize container
		String pwd = System.getProperty("user.dir");
		File privilegeContainerXml = new File(pwd + "/config/PrivilegeContainer.xml");
		PrivilegeContainer privilegeContainer = PrivilegeContainer.getInstance();
		privilegeContainer.initialize(privilegeContainerXml);
	}

	@Test
	public void testAuthenticationOk() throws Exception {

		Certificate certificate = PrivilegeContainer.getInstance().getSessionHandler().authenticate("eitch",
				"1234567890");
		org.junit.Assert.assertTrue("Certificate is null!", certificate != null);
	}

	@Test(expected = AccessDeniedException.class)
	public void testFailAuthenticationNOk() throws Exception {

		Certificate certificate = PrivilegeContainer.getInstance().getSessionHandler().authenticate("eitch", "123");
		org.junit.Assert.assertTrue("Certificate is null!", certificate != null);
	}

	@Test(expected = PrivilegeException.class)
	public void testFailAuthenticationPWNull() throws Exception {

		Certificate certificate = PrivilegeContainer.getInstance().getSessionHandler().authenticate("eitch", null);
		org.junit.Assert.assertTrue("Certificate is null!", certificate != null);
	}

	@Test
	public void testAddUserBobWithPW() throws Exception {

		Certificate certificate = PrivilegeContainer.getInstance().getSessionHandler().authenticate("eitch",
				"1234567890");

		ModelHandler modelHandler = PrivilegeContainer.getInstance().getModelHandler();

		// let's add a new user bob
		UserRep userRep = new UserRep("bob", "Bob", "Newman", UserState.NEW, new HashSet<String>(), null);
		modelHandler.addOrReplaceUser(certificate, userRep, null);
		logger.info("Added user bob");

		// set bob's password
		modelHandler.setUserPassword(certificate, "bob", "12345678901");
		logger.info("Set Bob's password");
	}

	/**
	 * Will fail because user bob is not yet enabled
	 * 
	 * @throws Exception
	 */
	@Test(expected = AccessDeniedException.class)
	public void testFailAuthAsBob() throws Exception {

		PrivilegeContainer.getInstance().getSessionHandler().authenticate("bob", "12345678901");
	}

	@Test
	public void testEnableUserBob() throws Exception {

		Certificate certificate = PrivilegeContainer.getInstance().getSessionHandler().authenticate("eitch",
				"1234567890");

		ModelHandler modelHandler = PrivilegeContainer.getInstance().getModelHandler();
		modelHandler.setUserState(certificate, "bob", UserState.ENABLED);
	}

	/**
	 * Will fail as user bob has no role
	 * 
	 * @throws Exception
	 */
	@Test(expected = PrivilegeException.class)
	public void testFailAuthUserBob() throws Exception {

		Certificate certificate = PrivilegeContainer.getInstance().getSessionHandler().authenticate("bob",
				"12345678901");
		org.junit.Assert.assertTrue("Certificate is null!", certificate != null);
	}

	@Test
	public void testAddUserRoleToBob() throws Exception {

		Certificate certificate = PrivilegeContainer.getInstance().getSessionHandler().authenticate("eitch",
				"1234567890");

		ModelHandler modelHandler = PrivilegeContainer.getInstance().getModelHandler();
		modelHandler.addRoleToUser(certificate, "bob", "user");
	}

	@Test
	public void testAuthAsBob() throws Exception {

		PrivilegeContainer.getInstance().getSessionHandler().authenticate("bob", "12345678901");
	}

	/**
	 * Will fail because user bob does not have admin rights
	 * 
	 * @throws Exception
	 */
	@Test(expected = AccessDeniedException.class)
	public void testFailAddUserTedAsBob() throws Exception {

		Certificate certificate = PrivilegeContainer.getInstance().getSessionHandler().authenticate("bob",
				"12345678901");
		org.junit.Assert.assertTrue("Certificate is null!", certificate != null);

		// let's add a new user bob
		UserRep userRep = new UserRep("bob", "Bob", "Newman", UserState.NEW, new HashSet<String>(), null);
		PrivilegeContainer.getInstance().getModelHandler().addOrReplaceUser(certificate, userRep, null);
		logger.info("Added user bob");
	}

	@Test
	public void testAddAdminRoleToBob() throws Exception {

		Certificate certificate = PrivilegeContainer.getInstance().getSessionHandler().authenticate("eitch",
				"1234567890");

		ModelHandler modelHandler = PrivilegeContainer.getInstance().getModelHandler();
		modelHandler.addRoleToUser(certificate, "bob", PrivilegeContainer.PRIVILEGE_ADMIN_ROLE);
	}

	@Test
	public void testAddUserTedAsBob() throws Exception {

		Certificate certificate = PrivilegeContainer.getInstance().getSessionHandler().authenticate("bob",
				"12345678901");
		org.junit.Assert.assertTrue("Certificate is null!", certificate != null);

		// let's add a new user ted
		UserRep userRep = new UserRep("ted", "Ted", "Newman", UserState.NEW, new HashSet<String>(), null);
		PrivilegeContainer.getInstance().getModelHandler().addOrReplaceUser(certificate, userRep, null);
		logger.info("Added user bob");
	}
}
