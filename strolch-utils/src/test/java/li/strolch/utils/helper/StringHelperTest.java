package li.strolch.utils.helper;

import org.junit.Test;

import static org.junit.Assert.*;

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

	@Test
	public void testIsUuid() {
		assertTrue(StringHelper.isUuid("550e8400-e29b-41d4-a716-446655440000"));
		assertTrue(StringHelper.isUuid("550E8400-E29B-41D4-A716-446655440000"));
		assertTrue(StringHelper.isUuid("00000000-0000-0000-0000-000000000000"));
		assertFalse(StringHelper.isUuid(null));
		assertFalse(StringHelper.isUuid(""));
		assertFalse(StringHelper.isUuid("admin"));
		assertFalse(StringHelper.isUuid("550e8400-e29b-41d4-a716-446655440000-extra"));
		assertFalse(StringHelper.isUuid("550e8400-e29b-41d4-a716-44665544000z"));
		assertFalse(StringHelper.isUuid("550e8400:e29b:41d4:a716:446655440000"));
	}
}
