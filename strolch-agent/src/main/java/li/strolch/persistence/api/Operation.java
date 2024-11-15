/*
 * Copyright (c) 2013-2024 Robert von Burg <eitch@eitchnet.ch>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

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
