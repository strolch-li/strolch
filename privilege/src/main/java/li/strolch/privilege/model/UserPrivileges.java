package li.strolch.privilege.model;

import java.util.List;

public record UserPrivileges(UserRep userRep, List<Privilege> privileges) {
	public UserPrivileges(UserRep userRep, List<Privilege> privileges) {
		this.userRep = userRep.readOnly();
		this.privileges = List.copyOf(privileges);
	}

	public <T> T accept(PrivilegeElementVisitor<T> visitor) {
		return visitor.visitUserPrivileges(this);
	}
}
