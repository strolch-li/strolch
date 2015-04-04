package li.strolch.service.privilege.roles;

import ch.eitchnet.privilege.model.PrivilegeRep;
import li.strolch.service.api.ServiceArgument;

public class PrivilegeAddOrReplacePrivilegeOnRoleArgument extends ServiceArgument {
	private static final long serialVersionUID = 1L;
	public String roleName;
	public PrivilegeRep privilegeRep;
}
