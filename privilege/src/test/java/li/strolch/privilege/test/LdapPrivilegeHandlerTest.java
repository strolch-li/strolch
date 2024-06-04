package li.strolch.privilege.test;

import org.junit.Assert;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;

public class LdapPrivilegeHandlerTest extends AbstractPrivilegeTest {

	@BeforeClass
	public static void init() throws IOException {
		removeConfigs(LdapPrivilegeHandlerTest.class.getSimpleName());
		prepareConfigs(LdapPrivilegeHandlerTest.class.getSimpleName(), "PrivilegeConfigJsonLdap.xml",
				"PrivilegeUsers.xml", "PrivilegeGroups.xml", "PrivilegeRoles.xml");
		Files.copy(new File(SRC_TEST_RESOURCES_CONFIG, "LdapGroupsConfig.json").toPath(),
				new File("target/" + LdapPrivilegeHandlerTest.class.getSimpleName(),
						"LdapGroupsConfig.json").toPath());
	}

	@Before
	public void setup() {
		initialize(LdapPrivilegeHandlerTest.class.getSimpleName(), "PrivilegeConfigJsonLdap.xml");
	}

	@Test
	public void shouldLoadHandler() {
		Assert.assertNotNull(this.privilegeHandler);
	}
}
