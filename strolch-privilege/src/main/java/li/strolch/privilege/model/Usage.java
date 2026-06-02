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

package li.strolch.privilege.model;

import li.strolch.privilege.base.PrivilegeException;

public enum Usage {
	ANY("any"),
	SINGLE("single"),
	SET_PASSWORD("set-password"),
	API("api");

	private final String value;

	Usage(String value) {
		this.value = value;
	}

	public String getValue() {
		return this.value;
	}

	public boolean isAny() {
		return this == ANY;
	}

	public boolean isSingle() {
		return this == SINGLE;
	}

	public boolean isSetPassword() {
		return this == SET_PASSWORD;
	}

	public boolean isApi() {
		return this == API;
	}

	public static Usage byValue(String value) {
		for (Usage usage : values()) {
			if (usage.value.equals(value))
				return usage;
		}

		throw new PrivilegeException("No Usage found with value: " + value);
	}
}
