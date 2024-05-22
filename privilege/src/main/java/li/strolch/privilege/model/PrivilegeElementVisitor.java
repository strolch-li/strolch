package li.strolch.privilege.model;

public interface PrivilegeElementVisitor<T> {

	T visitUserRep(UserRep userRep);

	T visitRoleRep(RoleRep roleRep);

	T visitGroup(Group group);

	T visitPrivilegeRep(Privilege privilege);

	T visitUserPrivileges(UserPrivileges userPrivileges);
}
