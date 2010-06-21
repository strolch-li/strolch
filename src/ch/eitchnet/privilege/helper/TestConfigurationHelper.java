/*
 * Copyright (c) 2010
 * 
 * Robert von Burg
 * eitch@eitchnet.ch
 * 
 * All rights reserved.
 * 
 */

package ch.eitchnet.privilege.helper;

import java.io.File;
import java.util.HashSet;

import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.ConsoleAppender;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.apache.log4j.PatternLayout;

import ch.eitchnet.privilege.base.PrivilegeContainer;
import ch.eitchnet.privilege.handler.ModelHandler;
import ch.eitchnet.privilege.model.Certificate;
import ch.eitchnet.privilege.model.UserRep;
import ch.eitchnet.privilege.model.UserState;

/**
 * @author rvonburg
 * 
 */
public class TestConfigurationHelper {
	private static final Logger logger = Logger.getLogger(TestConfigurationHelper.class);

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		BasicConfigurator.resetConfiguration();
		BasicConfigurator.configure(new ConsoleAppender(new PatternLayout("%d %5p [%t] %C{1} %M - %m%n")));
		Logger.getRootLogger().setLevel(Level.INFO);

		// initialize container
		String pwd = System.getProperty("user.dir");
		File privilegeContainerXml = new File(pwd + "/config/PrivilegeContainer.xml");
		PrivilegeContainer privilegeContainer = PrivilegeContainer.getInstance();
		privilegeContainer.initialize(privilegeContainerXml);

		ModelHandler modelHandler = privilegeContainer.getModelHandler();

		Certificate certificate = auth("eitch", "1234567890");

		for (int i = 0; i < 10; i++) {
			// let's authenticate a session
			auth("eitch", "1234567890");
		}

		// let's add a new user bob
		UserRep userRep = new UserRep("bob", "Bob", "Newman", UserState.NEW, new HashSet<String>(), null);
		modelHandler.addOrReplaceUser(certificate, userRep, null);
		logger.info("Added user bob");

		// TODO let's add a role

		// TODO let's add a privilege

	}

	/**
	 * 
	 */
	private static Certificate auth(String username, String password) {
		long start = System.currentTimeMillis();
		Certificate certificate = PrivilegeContainer.getInstance().getSessionHandler().authenticate(username, password);
		logger.info("Auth took " + (System.currentTimeMillis() - start));
		logger.info("Authenticated with certificate: " + certificate);
		return certificate;
	}
}
