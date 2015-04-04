package li.strolch.service.privilege.roles;

import li.strolch.service.api.ServiceResult;
import li.strolch.service.api.ServiceResultState;
import ch.eitchnet.privilege.model.RoleRep;

public class PrivilegeRoleResult extends ServiceResult {
	private static final long serialVersionUID = 1L;

	private RoleRep role;

	public PrivilegeRoleResult() {
		super();
	}

	public PrivilegeRoleResult(ServiceResultState state, String message) {
		super(state, message);
	}

	public PrivilegeRoleResult(ServiceResultState state) {
		super(state);
	}

	public PrivilegeRoleResult(RoleRep role) {
		setState(ServiceResultState.SUCCESS);
		this.role = role;
	}

	public RoleRep getRole() {
		return role;
	}
}
