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

package li.strolch.utils;

import org.junit.Test;

import static org.junit.Assert.*;

public class CodeGeneratorTest {

	@Test
	public void shouldCreateGeneratorLowerUpper() {
		String code = CodeGenerator.alphaNumericLowerUpper(500);
		assertEquals(500, code.length());
		assertFalse(code.contains("0"));
		assertFalse(code.contains("i"));
		assertFalse(code.contains("I"));
		assertFalse(code.contains("l"));
		assertFalse(code.contains("o"));
		assertFalse(code.contains("O"));
	}

	@Test
	public void shouldCreateGeneratorUpper() {
		String code = CodeGenerator.alphaNumericUpper(500);
		assertEquals(500, code.length());
		assertFalse(code.contains("0"));
		assertFalse(code.contains("i"));
		assertFalse(code.contains("I"));
		assertFalse(code.contains("l"));
		assertFalse(code.contains("o"));
		assertFalse(code.contains("O"));

		for (int i = 0; i < code.length(); i++) {
			char c = code.charAt(i);
			assertTrue(Character.isDigit(c) || Character.isUpperCase(c));
		}
	}
}
