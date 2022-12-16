package li.strolch.privilege.test;

import java.util.HashMap;
import java.util.Map;

import li.strolch.privilege.model.Certificate;
import li.strolch.privilege.model.Restrictable;
import li.strolch.privilege.test.model.TestRestrictable;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

public class SsoHandlerTest extends AbstractPrivilegeTest {

	@BeforeClass
	public static void init() throws Exception {
		removeConfigs(SsoHandlerTest.class.getSimpleName());
		prepareConfigs(SsoHandlerTest.class.getSimpleName(), "PrivilegeConfig.xml", "PrivilegeUsers.xml",
				"PrivilegeRoles.xml");
	}

	@AfterClass
	public static void destroy() throws Exception {
		removeConfigs(SsoHandlerTest.class.getSimpleName());
	}

	@Before
	public void setup() throws Exception {
		initialize(SsoHandlerTest.class.getSimpleName(), "PrivilegeConfig.xml");
	}

	@Test
	public void testSsoAdmin() throws Exception {

		try {
			Map<String, String> data = new HashMap<>();
			data.put("userId", "admin");
			data.put("username", "admin");
			data.put("firstName", "Admin");
			data.put("lastName", "Istrator");
			data.put("roles", "PrivilegeAdmin, AppUser");

			// auth
			Certificate cert = this.privilegeHandler.authenticateSingleSignOn(data, false);
			this.ctx = this.privilegeHandler.validate(cert);

			// validate action
			Restrictable restrictable = new TestRestrictable();
			this.ctx.validateAction(restrictable);

		} finally {
			// de-auth
			logout();
		}
	}
}
