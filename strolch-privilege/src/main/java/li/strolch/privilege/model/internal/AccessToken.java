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

import li.strolch.privilege.model.Privilege;

import java.time.ZonedDateTime;
import java.util.Map;

public record AccessToken(String tokenId, String username, PasswordCrypt passwordCrypt, ZonedDateTime validFrom,
						  ZonedDateTime validTo, Map<String, Privilege> privileges) {

	public AccessToken(String tokenId, String username, PasswordCrypt passwordCrypt, ZonedDateTime validFrom,
			ZonedDateTime validTo, Map<String, Privilege> privileges) {
		this.tokenId = tokenId;
		this.username = username;
		this.passwordCrypt = passwordCrypt;
		this.validFrom = validFrom;
		this.validTo = validTo;
		this.privileges = Map.copyOf(privileges);
	}

	@Override
	public String toString() {
		return "AccessToken{tokenId='%s', username='%s', validFrom=%s, validTo=%s, privileges=%s}".formatted(tokenId,
				username, validFrom, validTo, privileges.size());
	}
}
