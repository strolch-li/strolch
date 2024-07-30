package li.strolch.privilege.test;

import li.strolch.privilege.base.PrivilegeException;
import li.strolch.privilege.handler.PrivilegeHandler;
import li.strolch.privilege.helper.PrivilegeInitializer;
import li.strolch.privilege.model.Certificate;
import li.strolch.privilege.model.PrivilegeContext;
import li.strolch.utils.helper.FileHelper;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.nio.file.Files;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;

import static org.junit.Assert.assertNotNull;

public class AbstractPrivilegeTest {

	protected static final Logger logger = LoggerFactory.getLogger(AbstractPrivilegeTest.class);
	public static final String SRC_TEST_RESOURCES_CONFIG = "src/test/resources/config";

	protected static ScheduledExecutorService executorService;
	protected PrivilegeHandler privilegeHandler;
	protected PrivilegeContext ctx;

	@BeforeClass
	public static void beforeClass() {
		executorService = Executors.newScheduledThreadPool(1);
	}

	@AfterClass
	public static void afterClass() {
		if (executorService != null)
			executorService.shutdownNow();
	}

	protected void login(String username, char[] password) {
		Certificate certificate = privilegeHandler.authenticate(username, password, false);
		assertNotNull("Certificate is null!", certificate);
		this.ctx = privilegeHandler.validate(certificate);
	}

	protected void logout() {
		if (this.ctx != null) {
			try {
				PrivilegeContext privilegeContext = this.ctx;
				this.ctx = null;
				privilegeHandler.invalidate(privilegeContext.getCertificate());
			} catch (PrivilegeException e) {
				String msg = "There is no PrivilegeContext currently bound to the ThreadLocal!";
				if (!e.getMessage().equals(msg))
					throw e;
			}
		}
	}

	protected static void prepareConfigs(String dst, String configFilename, String usersFilename, String groupsFilename,
			String rolesFilename) {
		try {
			File configPath = new File(SRC_TEST_RESOURCES_CONFIG);

			File privilegeConfigFile = new File(configPath, configFilename);
			File privilegeUsersFile = new File(configPath, usersFilename);
			File privilegeGroupsFile = new File(configPath, groupsFilename);
			File privilegeRolesFile = new File(configPath, rolesFilename);

			File targetPath = new File("target/" + dst);
			if (!targetPath.mkdirs())
				throw new RuntimeException("Could not create parent " + targetPath);

			File dstConfig = new File(targetPath, configFilename);
			File dstUsers = new File(targetPath, usersFilename);
			File dstGroups = new File(targetPath, groupsFilename);
			File dstRoles = new File(targetPath, rolesFilename);

			// write config
			String config = Files.readString(privilegeConfigFile.toPath());
			config = config.replace("${target}", dst);
			Files.writeString(dstConfig.toPath(), config);

			// copy model
			Files.copy(privilegeUsersFile.toPath(), dstUsers.toPath());
			Files.copy(privilegeGroupsFile.toPath(), dstGroups.toPath());
			Files.copy(privilegeRolesFile.toPath(), dstRoles.toPath());

		} catch (Exception e) {
			logger.error(e.getMessage(), e);
			throw new RuntimeException("Initialization failed", e);
		}
	}

	protected static void removeConfigs(String dst) {
		try {
			File targetPath = new File("target");
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
			File targetPath = new File("target");
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
			this.privilegeHandler = new PrivilegeInitializer(executorService).initializeFromXml(privilegeConfigFile);
		} catch (Exception e) {
			logger.error(e.getMessage(), e);
			throw new RuntimeException("Initialization failed", e);
		}
	}
}
