package li.strolch.privilege.helper;

import static li.strolch.utils.LdapHelper.encodeForLDAP;

public class LinuxLdapQuery extends WindowsLdapQuery {
	public LinuxLdapQuery(String providerUrl, String searchBase, String additionalFilter, String domain,
			String domainPrefix) {
		super(providerUrl, searchBase, additionalFilter, domain, domainPrefix);
	}

	@Override
	protected String getDistinguishedName(String safeUsername) {
		return "uid=" + safeUsername + "," + this.searchBase;
	}

	@Override
	protected String getUserAttributeIdentifier1() {
		return "uid";
	}

	@Override
	protected String getObjectClassFilter() {
		return "(objectClass=person)";
	}
}
