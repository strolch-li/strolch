/*
 * Copyright 2013 Robert von Burg <eitch@eitchnet.ch>
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
package li.strolch.privilege.base;

import li.strolch.privilege.model.internal.Role;
import li.strolch.privilege.model.internal.User;

/**
 * The {@link PrivilegeConflictResolution} defines what should be done if a {@link User} has {@link Role Roles} which
 * have Privileges with conflicting names.
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public enum PrivilegeConflictResolution {

	/**
	 * STRICT requires that a User may not have conflicting Privileges throug multiple Roles. In this case an Exception
	 * is thrown.
	 */
	STRICT {
		@Override
		public boolean isStrict() {
			return true;
		}
	},

	/**
	 * MERGE defines that if conflicting privileges are encountered then a merge is to take place. A merge means that if
	 * all is allowed, then that wins. Otherwise any existing allow and deny lists are merged
	 */
	MERGE {
		@Override
		public boolean isStrict() {
			return false;
		}
	};

	public abstract boolean isStrict();
}
