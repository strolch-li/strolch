package li.strolch.privilege.test;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.HashMap;
import java.util.Map;

import li.strolch.privilege.handler.BasicPasswordStrengthHandler;
import li.strolch.privilege.handler.PasswordStrengthHandler;
import org.junit.Test;

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
