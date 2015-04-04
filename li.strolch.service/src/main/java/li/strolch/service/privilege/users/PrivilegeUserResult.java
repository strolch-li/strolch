package li.strolch.service.privilege.users;

import li.strolch.service.api.ServiceResult;
import li.strolch.service.api.ServiceResultState;
import ch.eitchnet.privilege.model.UserRep;

public class PrivilegeUserResult extends ServiceResult {
	private static final long serialVersionUID = 1L;

	private UserRep user;

	public PrivilegeUserResult() {
		super();
	}

	public PrivilegeUserResult(ServiceResultState state, String message) {
		super(state, message);
	}

	public PrivilegeUserResult(ServiceResultState state) {
		super(state);
	}

	public PrivilegeUserResult(UserRep user) {
		setState(ServiceResultState.SUCCESS);
		this.user = user;
	}

	public UserRep getUser() {
		return user;
	}
}
