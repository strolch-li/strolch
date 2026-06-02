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
package li.strolch.privilege.model;

import li.strolch.utils.dbc.DBC;

import java.time.ZonedDateTime;
import java.util.List;
import java.util.Set;

public class CreatePersonalAccessTokenArgument {
	public String name;
	public ZonedDateTime validFrom;
	public ZonedDateTime validTo;
	public Set<String> roles;
	public Set<String> privileges;

	public void validate() {
		DBC.PRE.assertNotEmpty("name must be set", name);
		DBC.PRE.assertNotNull("validFrom must be set", validFrom);
		DBC.PRE.assertNotNull("validTo must be set", validTo);
	}
}
