package li.strolch.privilege.helper;

import javax.naming.NamingException;
import javax.naming.directory.Attribute;
import javax.naming.directory.Attributes;
import javax.naming.ldap.LdapName;
import javax.naming.ldap.Rdn;
import java.util.HashSet;
import java.util.Set;

public class LdapHelper {

	public static Set<String> getLdapGroups(Attributes attrs) throws NamingException {
		Set<String> ldapRoles = new HashSet<>();
		Attribute groupMembers = attrs.get("memberOf");
		if (groupMembers == null)
			return ldapRoles;

		for (int i = 0; i < groupMembers.size(); i++) {
			String memberOfLdapString = attrs.get("memberOf").get(i).toString();

			// extract group name from ldap string -> CN=groupname,OU=company,DC=domain,DC=country
			LdapName memberOfName = new LdapName(memberOfLdapString);
			for (Rdn rdn : memberOfName.getRdns()) {
				if (rdn.getType().equalsIgnoreCase("CN")) {
					String groupName = rdn.getValue().toString();
					ldapRoles.add(groupName);
					break;
				}
			}
		}

		return ldapRoles;
	}
}
