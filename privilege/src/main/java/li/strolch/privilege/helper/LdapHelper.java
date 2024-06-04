package li.strolch.privilege.helper;

import javax.naming.NamingException;
import javax.naming.directory.Attribute;
import javax.naming.directory.Attributes;
import javax.naming.ldap.LdapName;
import javax.naming.ldap.Rdn;
import java.util.HashSet;
import java.util.Set;

public class LdapHelper {

	public static final String LDAP_SN = "sn";
	public static final String LDAP_DEPARTMENT = "department";
	public static final String LDAP_MEMBER_OF = "memberOf";
	public static final String LDAP_CN = "CN";


	public static Set<String> getLdapGroups(Attributes attrs) throws NamingException {
		Set<String> ldapRoles = new HashSet<>();
		Attribute groupMembers = attrs.get(LDAP_MEMBER_OF);
		if (groupMembers == null)
			return ldapRoles;

		for (int i = 0; i < groupMembers.size(); i++) {
			String memberOfLdapString = attrs.get(LDAP_MEMBER_OF).get(i).toString();

			// extract group name from ldap string -> CN=groupname,OU=company,DC=domain,DC=country
			LdapName memberOfName = new LdapName(memberOfLdapString);
			for (Rdn rdn : memberOfName.getRdns()) {
				if (rdn.getType().equalsIgnoreCase(LDAP_CN)) {
					String groupName = rdn.getValue().toString();
					ldapRoles.add(groupName);
					break;
				}
			}
		}

		return ldapRoles;
	}
}
