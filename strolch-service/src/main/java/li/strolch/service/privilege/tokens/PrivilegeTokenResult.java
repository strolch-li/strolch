/*
 * Copyright (c) 2015-2025 Robert von Burg <eitch@eitchnet.ch>
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
package li.strolch.service.privilege.tokens;

import li.strolch.privilege.model.PersonalAccessTokenRep;
import li.strolch.service.api.ServiceResult;
import li.strolch.service.api.ServiceResultState;

import java.util.List;

public class PrivilegeTokenResult extends ServiceResult {
	private PersonalAccessTokenRep token;
	private List<PersonalAccessTokenRep> tokens;
	private String rawToken;

	public PrivilegeTokenResult() {
		super();
	}

	public PrivilegeTokenResult(ServiceResultState state) {
		super(state);
	}

	public PrivilegeTokenResult(PersonalAccessTokenRep token) {
		setState(ServiceResultState.SUCCESS);
		this.token = token;
	}

	public PrivilegeTokenResult(List<PersonalAccessTokenRep> tokens) {
		setState(ServiceResultState.SUCCESS);
		this.tokens = tokens;
	}

	public PrivilegeTokenResult(String rawToken) {
		setState(ServiceResultState.SUCCESS);
		this.rawToken = rawToken;
	}

	public PersonalAccessTokenRep getToken() {
		return token;
	}

	public List<PersonalAccessTokenRep> getTokens() {
		return tokens;
	}

	public String getRawToken() {
		return rawToken;
	}
}
