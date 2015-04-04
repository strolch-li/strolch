package li.strolch.service.privilege.users;

import li.strolch.service.api.ServiceArgument;
import ch.eitchnet.privilege.model.UserState;

public class PrivilegeSetUserStateArgument extends ServiceArgument {
	private static final long serialVersionUID = 1L;
	public String username;
	public UserState userState;
}
