/*
 * Copyright 2015 Robert von Burg <eitch@eitchnet.ch>
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
package li.strolch.service.privilege.users;

import li.strolch.privilege.model.UserRep;
import li.strolch.service.api.ServiceResult;
import li.strolch.service.api.ServiceResultState;

public class PrivilegeUserResult extends ServiceResult {
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
