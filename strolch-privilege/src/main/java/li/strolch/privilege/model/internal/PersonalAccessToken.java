/*
 * Copyright (c) 2025 Robert von Burg <eitch@eitchnet.ch>
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

package li.strolch.privilege.model.internal;

import li.strolch.privilege.model.PersonalAccessTokenRep;
import li.strolch.privilege.model.Privilege;

import java.time.ZonedDateTime;
import java.util.List;
import java.util.Map;

public record PersonalAccessToken(String tokenId, String username, String name, PasswordCrypt passwordCrypt,
								  ZonedDateTime validFrom, ZonedDateTime validTo, ZonedDateTime lastUsed,
								  Map<String, Privilege> privileges) {

	public PersonalAccessToken(String tokenId, String username, String name, PasswordCrypt passwordCrypt,
			ZonedDateTime validFrom, ZonedDateTime validTo, ZonedDateTime lastUsed, Map<String, Privilege> privileges) {
		this.tokenId = tokenId;
		this.username = username;
		this.name = name;
		this.passwordCrypt = passwordCrypt;
		this.validFrom = validFrom;
		this.validTo = validTo;
		this.lastUsed = lastUsed;
		this.privileges = Map.copyOf(privileges);
	}

	public PersonalAccessToken withLastUsed(ZonedDateTime lastUsed) {
		return new PersonalAccessToken(this.tokenId, this.username, this.name, this.passwordCrypt, this.validFrom,
				this.validTo, lastUsed, this.privileges);
	}

	public PersonalAccessTokenRep asRep() {
		return new PersonalAccessTokenRep(this.tokenId, this.username, this.name, this.validFrom, this.validTo,
				this.lastUsed, List.copyOf(this.privileges.values()));
	}

	@Override
	public String toString() {
		return "PersonalAccessToken{tokenId='%s', username='%s', name='%s', validFrom=%s, validTo=%s, lastUsed=%s, privileges=%s}".formatted(
				tokenId, username, name, validFrom, validTo, lastUsed, privileges.size());
	}
}
