package li.strolch.service.privilege.roles;

import li.strolch.service.api.ServiceArgument;

public class PrivilegeRemovePrivilegeFromRoleArgument extends ServiceArgument {
	private static final long serialVersionUID = 1L;
	public String roleName;
	public String privilegeName;
}
