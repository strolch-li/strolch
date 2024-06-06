package li.strolch.privilege.test;

import li.strolch.privilege.helper.LinuxLdapQuery;
import org.junit.Assert;
import org.junit.Test;

import javax.naming.NamingException;
import javax.naming.directory.SearchResult;

public class LdapIntegrationTest {

	@Test
	public void testLdap() throws NamingException {

		String providerUrl = "ldap://localhost:389";
		String searchBase = "ou=People,dc=atexxi,dc=ch";
		String additionalFilter = "(memberOf=cn=Accounts,ou=Groups,dc=atexxi,dc=ch)";
		String domain = "";
		String domainPrefix = "";

		String username = "eitch";
		char[] password = "eitch".toCharArray();

		SearchResult searchResult;
		try (LinuxLdapQuery query = new LinuxLdapQuery(providerUrl, searchBase, additionalFilter, domain,
				domainPrefix)) {
			searchResult = query.searchLdap(username, password);
		}

		Assert.assertNotNull(searchResult);
	}

}
