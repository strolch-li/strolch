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

package li.strolch.privilege.test;

import li.strolch.privilege.handler.BasicPasswordStrengthHandler;
import li.strolch.privilege.handler.PasswordStrengthHandler;
import org.junit.Test;

import java.util.HashMap;
import java.util.Map;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class BasicPasswordStrengthHandlerTest {

	@Test
	public void testPwStrengthBasic() {
		PasswordStrengthHandler handler = new BasicPasswordStrengthHandler();
		Map<String, String> parameters = new HashMap<>();
		parameters.put("minLength", "8");
		parameters.put("maxLength", "1024");
		parameters.put("needsNumbers", "true");
		parameters.put("needsLowerCase", "true");
		parameters.put("needsUpperCase", "true");
		parameters.put("needsSpecialChars", "true");
		handler.initialize(parameters);

		assertTrue(handler.validateStrength("Testing0!".toCharArray()));
		assertTrue(handler.validateStrength("Täëing0!".toCharArray()));
		assertTrue(handler.validateStrength("Testing0@".toCharArray()));
		assertTrue(handler.validateStrength("Testing0¼".toCharArray()));
		assertTrue(handler.validateStrength("Testing0|".toCharArray()));
		assertTrue(handler.validateStrength("+n4lJ,7&".toCharArray()));
		assertTrue(handler.validateStrength("]}`aH1&z".toCharArray()));
		assertFalse(handler.validateStrength("Tg0!".toCharArray()));
	}

	@Test
	public void testPwStrengthOnlyNumbers() {
		PasswordStrengthHandler handler = new BasicPasswordStrengthHandler();
		Map<String, String> parameters = new HashMap<>();
		parameters.put("minLength", "8");
		parameters.put("maxLength", "8");
		parameters.put("needsNumbers", "true");
		parameters.put("needsLowerCase", "false");
		parameters.put("needsUpperCase", "false");
		parameters.put("needsSpecialChars", "false");
		handler.initialize(parameters);

		assertTrue(handler.validateStrength("34534534".toCharArray()));
		assertFalse(handler.validateStrength("Testing0!".toCharArray()));
	}
}
