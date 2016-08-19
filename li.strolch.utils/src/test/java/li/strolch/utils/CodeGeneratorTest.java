package li.strolch.utils;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

public class CodeGeneratorTest {

	@Test
	public void shouldCreateGeneratorLowerUpper() {
		String code = CodeGenerator.alphaNumericLowerUpper(12);
		assertEquals(12, code.length());
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
