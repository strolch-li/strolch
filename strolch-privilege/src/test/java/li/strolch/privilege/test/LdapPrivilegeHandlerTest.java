package li.strolch.privilege.test;

import li.strolch.privilege.model.Certificate;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

public class LdapPrivilegeHandlerTest extends AbstractPrivilegeTest {

	@BeforeClass
	public static void init() throws IOException {
		removeConfigs(LdapPrivilegeHandlerTest.class.getSimpleName());
		prepareConfigs(LdapPrivilegeHandlerTest.class.getSimpleName(), "PrivilegeConfigLdap.xml", "PrivilegeUsers.xml",
				"PrivilegeGroups.xml", "PrivilegeRoles.xml");
		Files.copy(new File(SRC_TEST_RESOURCES_CONFIG, "LdapGroupsConfig.json").toPath(),
				new File("target/" + LdapPrivilegeHandlerTest.class.getSimpleName(), "LdapGroupsConfig.json").toPath());
	}

	@Before
	public void setup() {
		initialize(LdapPrivilegeHandlerTest.class.getSimpleName(), "PrivilegeConfigLdap.xml");
	}

	@Test
	public void shouldLoadHandler() {
		assertNotNull(this.privilegeHandler);
	}

	@Ignore("This test requires a running LDAP server")
	@Test
	public void shouldLoginTest1() {
		Certificate cert = this.privilegeHandler.authenticate("test1", "test".toCharArray(), false);
		assertNotNull(cert);

		assertEquals("LocationA", cert.getLocation());
	}

	@Ignore("This test requires a running LDAP server")
	@Test
	public void shouldLoginTest2() {
		Certificate cert = this.privilegeHandler.authenticate("test2", "test".toCharArray(), false);
		assertNotNull(cert);

		assertEquals("LocationA,LocationB", cert.getLocation());
	}
}
