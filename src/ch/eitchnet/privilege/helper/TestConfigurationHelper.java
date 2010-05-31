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

import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.ConsoleAppender;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.apache.log4j.PatternLayout;

import ch.eitchnet.privilege.base.PrivilegeContainer;
import ch.eitchnet.privilege.model.Certificate;

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
		PrivilegeContainer.getInstance().initialize(privilegeContainerXml);

		for (int i = 0; i < 10; i++) {
			// let's authenticate a session
			auth("eitch", "1234567890");
		}
	}

	/**
	 * 
	 */
	private static void auth(String username, String password) {
		long start = System.currentTimeMillis();
		Certificate certificate = PrivilegeContainer.getInstance().getSessionHandler().authenticate(username, password);
		logger.info("Auth took " + (System.currentTimeMillis() - start));
		logger.info("Authenticated with certificate: " + certificate);
	}
}
