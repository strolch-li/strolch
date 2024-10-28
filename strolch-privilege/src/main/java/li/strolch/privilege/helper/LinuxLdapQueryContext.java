package li.strolch.privilege.helper;

import li.strolch.privilege.handler.WindowsLdapQueryContext;

import javax.naming.NamingException;
import javax.naming.directory.Attributes;
import java.util.Map;

public class LinuxLdapQueryContext extends WindowsLdapQueryContext {

	public LinuxLdapQueryContext(Map<String, String> parameterMap, RemoteGroupMappingModel groupMappingModel) {
		super(parameterMap, groupMappingModel);
	}

	@Override
	public String getDistinguishedName(String safeUsername) {
		return "uid=" + safeUsername + "," + this.searchBase;
	}

	@Override
	public String validateLdapUsername(String username, Attributes attrs) throws NamingException {
		return super.validateLdapUsername(username, attrs);
	}

	@Override
	public String getUserAttributeIdentifier1() {
		return "uid";
	}

	@Override
	public String getObjectClassFilter() {
		return "(objectClass=person)";
	}
}
