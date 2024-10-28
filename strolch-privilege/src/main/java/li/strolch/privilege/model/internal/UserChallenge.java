/*
 * Copyright (c) 2024 Robert von Burg <eitch@eitchnet.ch>
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

import li.strolch.privilege.model.Usage;
import li.strolch.utils.dbc.DBC;

import java.time.LocalDateTime;

public final class UserChallenge {
	private final User user;
	private final String challenge;
	private final String source;
	private final LocalDateTime initiated;
	private final Usage usage;
	private boolean fulfilled;

	public UserChallenge(Usage usage, User user, String challenge, String source) {
		DBC.PRE.assertNotNull("usage may not be null", usage);
		DBC.PRE.assertNotNull("user may not be null", user);
		DBC.PRE.assertNotNull("challenge may not be empty", challenge);
		DBC.PRE.assertNotNull("source may not be empty", source);
		this.usage = usage;
		this.user = user;
		this.challenge = challenge;
		this.source = source;
		this.initiated = LocalDateTime.now();
	}

	public Usage getUsage() {
		return this.usage;
	}

	public User getUser() {
		return this.user;
	}

	public String getChallenge() {
		return this.challenge;
	}

	public String getSource() {
		return this.source;
	}

	public LocalDateTime getInitiated() {
		return this.initiated;
	}

	public boolean isFulfilled() {
		return this.fulfilled;
	}

	public void fulfilled() {
		this.fulfilled = true;
	}
}