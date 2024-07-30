package li.strolch.privilege.helper;

import java.util.Set;

public record GroupsAndRoles(Set<String> groups, Set<String> roles) {
	public boolean isEmpty() {
		return this.groups.isEmpty() && this.roles.isEmpty();
	}
}