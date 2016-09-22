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

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

import li.strolch.utils.StringMatchMode;

/**
 * @author Robert von Burg &lt;eitch@eitchnet.ch&gt;
 *
 */
public class StringMatchModeTest {

	/**
	 * Test method for {@link li.strolch.utils.StringMatchMode#isCaseSensitve()}.
	 */
	@Test
	public void testIsCaseSensitve() {
		assertFalse(StringMatchMode.EQUALS_CASE_INSENSITIVE.isCaseSensitve());
		assertTrue(StringMatchMode.EQUALS_CASE_SENSITIVE.isCaseSensitve());
		assertFalse(StringMatchMode.CONTAINS_CASE_INSENSITIVE.isCaseSensitve());
		assertTrue(StringMatchMode.CONTAINS_CASE_SENSITIVE.isCaseSensitve());
	}

	/**
	 * Test method for {@link li.strolch.utils.StringMatchMode#isEquals()}.
	 */
	@Test
	public void testIsEquals() {
		assertTrue(StringMatchMode.EQUALS_CASE_INSENSITIVE.isEquals());
		assertTrue(StringMatchMode.EQUALS_CASE_SENSITIVE.isEquals());
		assertFalse(StringMatchMode.CONTAINS_CASE_INSENSITIVE.isEquals());
		assertFalse(StringMatchMode.CONTAINS_CASE_SENSITIVE.isEquals());
	}

	/**
	 * Test method for {@link li.strolch.utils.StringMatchMode#matches(java.lang.String, java.lang.String)}.
	 */
	@SuppressWarnings("nls")
	@Test
	public void testMatches() {
		assertTrue(StringMatchMode.CONTAINS_CASE_INSENSITIVE.matches("hello", "el"));
		assertTrue(StringMatchMode.CONTAINS_CASE_SENSITIVE.matches("hello", "el"));
		assertFalse(StringMatchMode.CONTAINS_CASE_SENSITIVE.matches("hello", "ael"));
		assertFalse(StringMatchMode.CONTAINS_CASE_SENSITIVE.matches("hello", "EL"));
		assertFalse(StringMatchMode.CONTAINS_CASE_SENSITIVE.matches("hello", "aEL"));
		assertTrue(StringMatchMode.CONTAINS_CASE_INSENSITIVE.matches("hello", "EL"));

		assertTrue(StringMatchMode.EQUALS_CASE_SENSITIVE.matches("ab", "ab"));
		assertFalse(StringMatchMode.EQUALS_CASE_SENSITIVE.matches("ab", "abc"));
		assertTrue(StringMatchMode.EQUALS_CASE_INSENSITIVE.matches("ab", "ab"));
		assertTrue(StringMatchMode.EQUALS_CASE_INSENSITIVE.matches("ab", "AB"));
		assertTrue(StringMatchMode.EQUALS_CASE_INSENSITIVE.matches("ab", "aB"));

		assertFalse(StringMatchMode.EQUALS_CASE_SENSITIVE.matches("ab", "aB"));
		assertFalse(StringMatchMode.EQUALS_CASE_SENSITIVE.matches("ab", "aB"));
		assertFalse(StringMatchMode.EQUALS_CASE_SENSITIVE.matches("ab", "AB"));
		assertFalse(StringMatchMode.EQUALS_CASE_SENSITIVE.matches("ab", "aba"));
		assertTrue(StringMatchMode.EQUALS_CASE_SENSITIVE.matches("ab", "ab"));
	}
}
