/*
 * Copyright 2013 Robert von Burg <eitch@eitchnet.ch>
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *     http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package ch.eitchnet.privilege.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.io.File;

import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ch.eitchnet.privilege.base.PrivilegeException;
import ch.eitchnet.privilege.handler.PrivilegeHandler;
import ch.eitchnet.privilege.helper.PrivilegeInitializationHelper;
import ch.eitchnet.privilege.model.Certificate;
import ch.eitchnet.privilege.model.IPrivilege;
import ch.eitchnet.privilege.model.PrivilegeContext;
import ch.eitchnet.utils.helper.FileHelper;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class PrivilegeConflictMergeTest {

	private static final Logger logger = LoggerFactory.getLogger(PrivilegeConflictMergeTest.class);

	@BeforeClass
	public static void init() throws Exception {
		try {
			destroy();

			// copy configuration to tmp
			String pwd = System.getProperty("user.dir");

			File origPrivilegeModelFile = new File(pwd + "/config/PrivilegeModelMerge.xml");
			File tmpPrivilegeModelFile = new File(pwd + "/target/testPrivilege/PrivilegeModelMerge.xml");
			if (tmpPrivilegeModelFile.exists() && !tmpPrivilegeModelFile.delete()) {
				throw new RuntimeException("Tmp configuration still exists and can not be deleted at "
						+ tmpPrivilegeModelFile.getAbsolutePath());
			}

			File parentFile = tmpPrivilegeModelFile.getParentFile();
			if (!parentFile.exists()) {
				if (!parentFile.mkdirs())
					throw new RuntimeException("Could not create parent for tmp " + tmpPrivilegeModelFile);
			}

			if (!FileHelper.copy(origPrivilegeModelFile, tmpPrivilegeModelFile, true))
				throw new RuntimeException("Failed to copy " + origPrivilegeModelFile + " to " + tmpPrivilegeModelFile);

		} catch (Exception e) {
			logger.error(e.getMessage(), e);

			throw new RuntimeException("Initialization failed: " + e.getLocalizedMessage(), e);
		}
	}

	@AfterClass
	public static void destroy() throws Exception {

		// delete temporary file
		String pwd = System.getProperty("user.dir");

		File tmpPrivilegeModelFile = new File(pwd + "/target/testPrivilege/PrivilegeModelMerge.xml");
		if (tmpPrivilegeModelFile.exists() && !tmpPrivilegeModelFile.delete()) {
			throw new RuntimeException("Tmp configuration still exists and can not be deleted at "
					+ tmpPrivilegeModelFile.getAbsolutePath());
		}

		// and temporary parent
		File parentFile = tmpPrivilegeModelFile.getParentFile();
		if (parentFile.exists() && !parentFile.delete()) {
			throw new RuntimeException("Could not remove temporary parent for tmp " + tmpPrivilegeModelFile);
		}
	}

	private PrivilegeHandler privilegeHandler;
	private PrivilegeContext ctx;

	@Before
	public void setup() throws Exception {
		try {

			String pwd = System.getProperty("user.dir");

			File privilegeConfigFile = new File(pwd + "/config/PrivilegeConfigMerge.xml");

			// initialize privilege
			privilegeHandler = PrivilegeInitializationHelper.initializeFromXml(privilegeConfigFile);

		} catch (Exception e) {
			logger.error(e.getMessage(), e);

			throw new RuntimeException("Setup failed: " + e.getLocalizedMessage(), e);
		}
	}

	private void login(String username, byte[] password) {
		Certificate certificate = privilegeHandler.authenticate(username, password);
		assertTrue("Certificate is null!", certificate != null);
		PrivilegeContext privilegeContext = privilegeHandler.getPrivilegeContext(certificate);
		this.ctx = privilegeContext;
	}

	private void logout() {
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

	@Test
	public void shouldMergePrivileges1() {
		try {
			login("userA", "admin".getBytes());
			IPrivilege privilege = this.ctx.getPrivilege("Foo");
			assertTrue(privilege.isAllAllowed());
			assertTrue(privilege.getAllowList().isEmpty());
			assertTrue(privilege.getDenyList().isEmpty());

		} finally {
			logout();
		}
	}

	@Test
	public void shouldMergePrivileges2() {
		try {
			login("userB", "admin".getBytes());
			IPrivilege privilege = this.ctx.getPrivilege("Bar");
			assertFalse(privilege.isAllAllowed());
			assertEquals(2, privilege.getAllowList().size());
			assertEquals(2, privilege.getDenyList().size());
		} finally {
			logout();
		}
	}
}
