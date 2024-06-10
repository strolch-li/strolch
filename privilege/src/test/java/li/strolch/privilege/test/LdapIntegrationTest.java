package li.strolch.privilege.test;

import li.strolch.privilege.handler.XmlPersistenceHandler;
import li.strolch.privilege.helper.LinuxLdapQuery;
import li.strolch.privilege.helper.LinuxLdapQueryContext;
import li.strolch.privilege.helper.RemoteGroupMappingModel;
import li.strolch.privilege.model.internal.User;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;

import javax.naming.directory.SearchResult;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

import static li.strolch.privilege.handler.WindowsLdapQueryContext.*;
import static li.strolch.privilege.helper.XmlConstants.PARAM_BASE_PATH;
import static org.junit.Assert.*;

@Ignore("This test requires a running LDAP server")
public class LdapIntegrationTest {

	private static LinuxLdapQueryContext queryContext;

	@BeforeClass
	public static void setUpBeforeClass() {

		Map<String, String> parameterMap = new HashMap<>();
		parameterMap.put(PARAM_PROVIDER_URL, "ldap://localhost:10389");
		parameterMap.put(PARAM_SEARCH_BASE, "ou=People,dc=strolch,dc=li");
		parameterMap.put(PARAM_ADDITIONAL_FILTER, "(memberOf=cn=Accounts,ou=Groups,dc=strolch,dc=li)");
		parameterMap.put(PARAM_DOMAIN, "");
		parameterMap.put(PARAM_DEFAULT_LOCALE, Locale.ENGLISH.toLanguageTag());

		XmlPersistenceHandler persistenceHandler = new XmlPersistenceHandler();
		Map<String, String> persistenceParameters = new HashMap<>();
		persistenceParameters.put(PARAM_BASE_PATH, "src/test/resources/config");
		persistenceHandler.initialize(persistenceParameters);
		RemoteGroupMappingModel groupMappingModel = new RemoteGroupMappingModel(persistenceHandler);
		groupMappingModel.loadJsonConfig("defaultRealm", "src/test/resources/config", "LdapGroupsConfig.json");

		queryContext = new LinuxLdapQueryContext(parameterMap, groupMappingModel);
	}

	@Test
	public void testUserTest1() throws Exception {

		String username = "test1";
		char[] password = "test".toCharArray();

		SearchResult searchResult;
		try (LinuxLdapQuery query = new LinuxLdapQuery(queryContext)) {
			searchResult = query.searchLdap(username, password);
		}
		assertNotNull(searchResult);

		User user = queryContext.buildUserFromSearchResult(username, searchResult);
		assertNotNull(user);

		assertEquals("test1", user.getUsername());
		assertEquals("Test 1", user.getFirstname());
		assertEquals("User", user.getLastname());
		assertNull(user.getLocation());
	}

	@Test
	public void testUserTest2() throws Exception {

		String username = "test2";
		char[] password = "test".toCharArray();

		SearchResult searchResult;
		try (LinuxLdapQuery query = new LinuxLdapQuery(queryContext)) {
			searchResult = query.searchLdap(username, password);
		}
		assertNotNull(searchResult);

		User user = queryContext.buildUserFromSearchResult(username, searchResult);
		assertNotNull(user);

		assertEquals("test2", user.getUsername());
		assertEquals("Test 2", user.getFirstname());
		assertEquals("User", user.getLastname());
		assertNull(user.getLocation());
	}
}
