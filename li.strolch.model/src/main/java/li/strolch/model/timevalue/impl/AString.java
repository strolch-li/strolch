/*
 * Copyright 2013 Martin Smock <smock.martin@gmail.com>
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
package li.strolch.model.timevalue.impl;

import java.io.Serializable;

import li.strolch.utils.dbc.DBC;

/**
 * Wrapper for java.util.String object defining a inverse to support algebraic operations.
 *
 * @author Martin Smock <smock.martin@gmail.com>
 */
public class AString implements Serializable {

	private final String string;
	private final boolean inverse;

	public AString(final String string) {
		DBC.PRE.assertNotNull("Value may not be null!", string);
		DBC.PRE.assertFalse("Comma not allowed in value!", string.contains(","));
		this.string = string;
		this.inverse = false;
	}

	public AString(final String string, final boolean inverse) {
		DBC.PRE.assertNotNull("Value may not be null!", string);
		DBC.PRE.assertFalse("Comma not allowed in value!", string.contains(","));
		this.string = string;
		this.inverse = inverse;
	}

	public String getString() {
		return this.string;
	}

	public boolean isInverse() {
		return this.inverse;
	}

	public AString getInverse() {
		return new AString(this.string, !this.inverse);
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + (this.inverse ? 1231 : 1237);
		result = prime * result + ((this.string == null) ? 0 : this.string.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (obj == null) {
			return false;
		}
		if (getClass() != obj.getClass()) {
			return false;
		}
		AString other = (AString) obj;
		if (this.inverse != other.inverse) {
			return false;
		}
		if (this.string == null) {
			if (other.string != null) {
				return false;
			}
		} else if (!this.string.equals(other.string)) {
			return false;
		}
		return true;
	}

	@SuppressWarnings("nls")
	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append("AString [string=");
		sb.append(this.string);
		sb.append(", inverse=");
		sb.append(this.inverse);
		sb.append("]");
		return sb.toString();
	}
}
