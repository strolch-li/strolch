package li.strolch.privilege.model;

import java.util.List;

public record GroupPrivileges(Group group, List<Privilege> privileges) {
	public GroupPrivileges(Group group, List<Privilege> privileges) {
		this.group = group;
		this.privileges = List.copyOf(privileges);
	}

	public <T> T accept(PrivilegeElementVisitor<T> visitor) {
		return visitor.visitGroupPrivileges(this);
	}
}
