/*
 * Copyright (c) 2013-2025 Robert von Burg <eitch@eitchnet.ch>
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

package li.strolch.privilege.test.model;

import li.strolch.privilege.base.PrivilegeException;
import li.strolch.privilege.handler.PersistenceHandler;
import li.strolch.privilege.handler.SingleSignOnHandler;
import li.strolch.privilege.model.UserState;
import li.strolch.privilege.model.internal.User;
import li.strolch.privilege.model.internal.UserHistory;

import java.util.*;
import java.util.stream.Collectors;

public class DummySsoHandler implements SingleSignOnHandler {

	private Map<String, String> parameterMap;

	@Override
	public Map<String, String> getParameterMap() {
		return this.parameterMap;
	}

	@Override
	public void initialize(PersistenceHandler persistenceHandler, Map<String, String> parameterMap) {
		this.parameterMap = parameterMap;
	}

	@Override
	public User authenticateSingleSignOn(Object data) throws PrivilegeException {

		@SuppressWarnings("unchecked") Map<String, String> map = (Map<String, String>) data;

		Set<String> groups = Arrays.stream(map.get("groups").split(",")).map(String::trim).collect(Collectors.toSet());
		Set<String> roles = Arrays.stream(map.get("roles").split(",")).map(String::trim).collect(Collectors.toSet());
		Map<String, String> properties = new HashMap<>();
		return new User(null, map.get("username"), null, map.get("firstName"), map.get("lastName"), UserState.REMOTE,
				groups, roles, Locale.ENGLISH, properties, false, UserHistory.EMPTY);
	}
}
