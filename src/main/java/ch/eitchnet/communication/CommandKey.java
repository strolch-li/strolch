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
package ch.eitchnet.communication;

import ch.eitchnet.utils.helper.StringHelper;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class CommandKey {
	private final String key1;
	private final String key2;
	private final int hashCode;

	/**
	 * @param key1
	 * @param key2
	 */
	public CommandKey(String key1, String key2) {
		this.key1 = key1;
		this.key2 = key2;

		final int prime = 31;
		int result = 1;
		result = prime * result + ((this.key1 == null) ? 0 : this.key1.hashCode());
		result = prime * result + ((this.key2 == null) ? 0 : this.key2.hashCode());
		this.hashCode = result;
	}

	public static CommandKey key(String key1, String key2) {
		return new CommandKey(key1, key2);
	}

	@Override
	public int hashCode() {
		return this.hashCode;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		CommandKey other = (CommandKey) obj;
		return this.key1.equals(other.key1) && this.key2.equals(other.key2);
	}

	@Override
	public String toString() {
		return this.key1 + StringHelper.COLON + this.key2;
	}
}
