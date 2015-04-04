package li.strolch.service.privilege.users;

import li.strolch.service.api.ServiceArgument;

public class PrivilegeSetUserPasswordArgument extends ServiceArgument {
	private static final long serialVersionUID = 1L;
	public String username;
	public byte[] password;
}
