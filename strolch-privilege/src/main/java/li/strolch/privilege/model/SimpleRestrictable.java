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
package li.strolch.privilege.model;

import li.strolch.utils.dbc.DBC;

/**
 * Implements a simple restrictable, which returns the privilege name and value passed on construction
 */
public class SimpleRestrictable implements Restrictable {

	private final String name;
	private final Object value;

	/**
	 * @param name  the name of the privilege
	 * @param value the value allowed on the privilege
	 */
	public SimpleRestrictable(String name, Object value) {
		DBC.PRE.assertNotEmpty("name must not be empty", name);
		DBC.PRE.assertNotNull("value must not be null", value);
		this.name = name;
		this.value = value;
	}

	@Override
	public String getPrivilegeName() {
		return this.name;
	}

	@Override
	public Object getPrivilegeValue() {
		return this.value;
	}

	public static SimpleRestrictable restrictableOf(String name, Object value) {
		return new SimpleRestrictable(name, value);
	}
}