package li.strolch.persistence.api;

import static li.strolch.runtime.StrolchConstants.StrolchPrivilegeConstants.PRIVILEGE_ADD_PREFIX;
import static li.strolch.runtime.StrolchConstants.StrolchPrivilegeConstants.PRIVILEGE_GET_PREFIX;
import static li.strolch.runtime.StrolchConstants.StrolchPrivilegeConstants.PRIVILEGE_REMOVE_PREFIX;
import static li.strolch.runtime.StrolchConstants.StrolchPrivilegeConstants.PRIVILEGE_UPDATE_PREFIX;

import li.strolch.model.StrolchRootElement;

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
