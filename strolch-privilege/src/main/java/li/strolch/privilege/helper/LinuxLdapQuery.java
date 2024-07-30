package li.strolch.privilege.helper;

import li.strolch.privilege.handler.WindowsLdapQueryContext;

import static li.strolch.utils.LdapHelper.encodeForLDAP;

public class LinuxLdapQuery extends WindowsLdapQuery {
	public LinuxLdapQuery(LinuxLdapQueryContext queryContext) {
		super(queryContext);
	}


}
