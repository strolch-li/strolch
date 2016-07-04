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
package li.strolch.utils;

import li.strolch.utils.dbc.DBC;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public enum StringMatchMode {
	EQUALS_CASE_SENSITIVE(true, true),
	EQUALS_CASE_INSENSITIVE(true, false),
	CONTAINS_CASE_SENSITIVE(false, true),
	CONTAINS_CASE_INSENSITIVE(false, false);

	private final boolean equals;
	private final boolean caseSensitve;

	private StringMatchMode(boolean equals, boolean caseSensitive) {
		this.equals = equals;
		this.caseSensitve = caseSensitive;
	}

	/**
	 * @return the caseSensitve
	 */
	public boolean isCaseSensitve() {
		return this.caseSensitve;
	}

	/**
	 * @return the equals
	 */
	public boolean isEquals() {
		return this.equals;
	}

	public boolean matches(String value1, String value2) {
		DBC.PRE.assertNotNull("value1 must be set!", value1); //$NON-NLS-1$
		DBC.PRE.assertNotNull("value2 must be set!", value2); //$NON-NLS-1$
		if (!isEquals() && !isCaseSensitve())
			return value1.toLowerCase().contains(value2.toLowerCase());

		if (!isCaseSensitve())
			return value1.toLowerCase().equals(value2.toLowerCase());

		if (!isEquals())
			return value1.contains(value2);

		return value1.equals(value2);
	}

	/**
	 * <p>
	 * Returns {@link StringMatchMode#EQUALS_CASE_SENSITIVE}
	 * </p>
	 * 
	 * <p>
	 * Short method, useful for static imports, or simply for shorter code
	 * </p>
	 * 
	 * @return {@link StringMatchMode#EQUALS_CASE_SENSITIVE}
	 */
	public static StringMatchMode es() {
		return EQUALS_CASE_SENSITIVE;
	}

	/**
	 * <p>
	 * Returns {@link #EQUALS_CASE_INSENSITIVE}
	 * </p>
	 * 
	 * <p>
	 * Short method, useful for static imports, or simply for shorter code
	 * </p>
	 * 
	 * @return {@link #EQUALS_CASE_INSENSITIVE}
	 */
	public static StringMatchMode ei() {
		return EQUALS_CASE_INSENSITIVE;
	}

	/**
	 * <p>
	 * Returns {@link #CONTAINS_CASE_SENSITIVE}
	 * </p>
	 * 
	 * <p>
	 * Short method, useful for static imports, or simply for shorter code
	 * </p>
	 * 
	 * @return {@link #CONTAINS_CASE_SENSITIVE}
	 */
	public static StringMatchMode cs() {
		return CONTAINS_CASE_SENSITIVE;
	}

	/**
	 * <p>
	 * Returns {@link #CONTAINS_CASE_INSENSITIVE}
	 * </p>
	 * 
	 * <p>
	 * Short method, useful for static imports, or simply for shorter code
	 * </p>
	 * 
	 * @return {@link #CONTAINS_CASE_INSENSITIVE}
	 */
	public static StringMatchMode ci() {
		return CONTAINS_CASE_INSENSITIVE;
	}
}
