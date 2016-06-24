package ch.eitchnet.privilege.test;

import static org.junit.Assert.assertTrue;

import java.io.File;
import java.nio.file.Files;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ch.eitchnet.privilege.base.PrivilegeException;
import ch.eitchnet.privilege.handler.PrivilegeHandler;
import ch.eitchnet.privilege.helper.PrivilegeInitializationHelper;
import ch.eitchnet.privilege.model.Certificate;
import ch.eitchnet.privilege.model.PrivilegeContext;
import ch.eitchnet.utils.helper.FileHelper;

public class AbstractPrivilegeTest {

	protected static final Logger logger = LoggerFactory.getLogger(AbstractPrivilegeTest.class);

	protected PrivilegeHandler privilegeHandler;
	protected PrivilegeContext ctx;

	protected void login(String username, byte[] password) {
		Certificate certificate = privilegeHandler.authenticate(username, password);
		assertTrue("Certificate is null!", certificate != null);
		PrivilegeContext privilegeContext = privilegeHandler.getPrivilegeContext(certificate);
		this.ctx = privilegeContext;
	}

	protected void logout() {
		if (this.ctx != null) {
			try {
				PrivilegeContext privilegeContext = this.ctx;
				this.ctx = null;
				privilegeHandler.invalidateSession(privilegeContext.getCertificate());
			} catch (PrivilegeException e) {
				String msg = "There is no PrivilegeContext currently bound to the ThreadLocal!";
				if (!e.getMessage().equals(msg))
					throw e;
			}
		}
	}

	protected static void prepareConfigs(String dst, String configFilename, String usersFilename,
			String rolesFilename) {
		try {
			String pwd = System.getProperty("user.dir");

			File configPath = new File(pwd, "config");

			File privilegeConfigFile = new File(configPath, configFilename);
			File privilegeUsersFile = new File(configPath, usersFilename);
			File privilegeRolesFile = new File(configPath, rolesFilename);

			File targetPath = new File(pwd, "target/" + dst);
			if (!targetPath.mkdirs())
				throw new RuntimeException("Could not create parent " + targetPath);

			File dstConfig = new File(targetPath, configFilename);
			File dstUsers = new File(targetPath, usersFilename);
			File dstRoles = new File(targetPath, rolesFilename);

			// write config
			String config = new String(Files.readAllBytes(privilegeConfigFile.toPath()), "UTF-8");
			config = config.replace("${target}", dst);
			Files.write(dstConfig.toPath(), config.getBytes("UTF-8"));

			// copy model
			Files.copy(privilegeUsersFile.toPath(), dstUsers.toPath());
			Files.copy(privilegeRolesFile.toPath(), dstRoles.toPath());

		} catch (Exception e) {
			logger.error(e.getMessage(), e);
			throw new RuntimeException("Initialization failed", e);
		}
	}

	protected static void removeConfigs(String dst) {
		try {
			String pwd = System.getProperty("user.dir");
			File targetPath = new File(pwd, "target");
			targetPath = new File(targetPath, dst);
			if (targetPath.exists() && !FileHelper.deleteFile(targetPath, true)) {
				throw new RuntimeException(
						"Tmp configuration still exists and can not be deleted at " + targetPath.getAbsolutePath());
			}
		} catch (Exception e) {
			logger.error(e.getMessage(), e);
			throw new RuntimeException("Initialization failed", e);
		}
	}

	protected static File getPrivilegeConfigFile(String dst, String configFilename) {
		try {
			String pwd = System.getProperty("user.dir");
			File targetPath = new File(pwd, "target");
			targetPath = new File(targetPath, dst);
			return new File(targetPath, configFilename);
		} catch (Exception e) {
			logger.error(e.getMessage(), e);
			throw new RuntimeException("Initialization failed", e);
		}
	}

	protected void initialize(String dst, String configFilename) {
		try {
			File privilegeConfigFile = getPrivilegeConfigFile(dst, configFilename);
			this.privilegeHandler = PrivilegeInitializationHelper.initializeFromXml(privilegeConfigFile);
		} catch (Exception e) {
			logger.error(e.getMessage(), e);
			throw new RuntimeException("Initialization failed", e);
		}
	}
}
