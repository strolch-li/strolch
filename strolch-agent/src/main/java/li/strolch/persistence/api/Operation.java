package li.strolch.persistence.api;

import li.strolch.model.StrolchRootElement;

import static li.strolch.runtime.StrolchConstants.StrolchPrivilegeConstants.*;

public enum Operation {
	GET(PRIVILEGE_GET_PREFIX), //
	ADD(PRIVILEGE_ADD_PREFIX), // 
	UPDATE(PRIVILEGE_UPDATE_PREFIX), // 
	REMOVE(PRIVILEGE_REMOVE_PREFIX);

	private final String privilegePrefix;

	Operation(String privilegePrefix) {
		this.privilegePrefix = privilegePrefix;
	}

	public String getPrivilegeName(StrolchRootElement element) {
		return this.privilegePrefix + element.getObjectType();
	}

	public String getPrivilegePrefix() {
		return this.privilegePrefix;
	}
}
