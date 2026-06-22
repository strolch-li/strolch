package li.strolch.utils.helper;

import org.junit.Test;

import static org.junit.Assert.assertEquals;

public class StringHelperTest {

	@Test
	public void testReplaceWhitespaceAndSpecialCharactersWithUnderscore() {
		String str = "Hello World!";
		String expected = "Hello_World_";
		String actual = StringHelper.replaceWhitespaceAndSpecialCharactersWithUnderscore(str);
		assertEquals(expected, actual);
	}

	@Test
	public void testReplaceOnlySpecialCharacters() {
		String str = "!@#$%^&*()";
		String expected = "__________";
		String actual = StringHelper.replaceWhitespaceAndSpecialCharactersWithUnderscore(str);
		assertEquals(expected, actual);
	}

	@Test
	public void testReplaceMixedWhitespace() {
		String str = "Hello\tWorld\nTest";
		String expected = "Hello_World_Test";
		String actual = StringHelper.replaceWhitespaceAndSpecialCharactersWithUnderscore(str);
		assertEquals(expected, actual);
	}

	@Test
	public void testReplaceEmptyString() {
		String str = "";
		String expected = "";
		String actual = StringHelper.replaceWhitespaceAndSpecialCharactersWithUnderscore(str);
		assertEquals(expected, actual);
	}

	@Test
	public void testReplaceOnlyWhitespace() {
		String str = "   ";
		String expected = "___";
		String actual = StringHelper.replaceWhitespaceAndSpecialCharactersWithUnderscore(str);
		assertEquals(expected, actual);
	}

	@Test
	public void testReplaceNoSpecialCharactersOrWhitespace() {
		String str = "HelloWorld123";
		String expected = "HelloWorld123";
		String actual = StringHelper.replaceWhitespaceAndSpecialCharactersWithUnderscore(str);
		assertEquals(expected, actual);
	}

	@Test
	public void testReplaceConsecutiveSpecialCharacters() {
		String str = "Hello!!!World???";
		String expected = "Hello___World___";
		String actual = StringHelper.replaceWhitespaceAndSpecialCharactersWithUnderscore(str);
		assertEquals(expected, actual);
	}
}
