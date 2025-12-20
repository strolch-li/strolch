package li.strolch.utils.helper;

import org.junit.Test;

import java.util.Map;

import static org.junit.Assert.assertEquals;

public class StringHelperFormatTest {

	@Test
	public void replacesSinglePlaceholder() {
		String pattern = "Box #: {boxNo}";
		String result = StringHelper.format(pattern, Map.of("boxNo", 42));
		assertEquals("Box #: 42", result);
	}

	@Test
	public void leavesUnknownPlaceholderUntouched() {
		String pattern = "Box #: {boxNo}";
		String result = StringHelper.format(pattern, Map.of());
		assertEquals("Box #: {boxNo}", result);
	}

	@Test
	public void replacesMultiplePlaceholders() {
		String pattern = "{a}-{b}-{a}";
		String result = StringHelper.format(pattern, Map.of("a", 1, "b", 2));
		assertEquals("1-2-1", result);
	}

	@Test
	public void handlesNoPlaceholders() {
		String pattern = "No placeholders here";
		String result = StringHelper.format(pattern, Map.of());
		assertEquals("No placeholders here", result);
	}

	@Test
	public void handlesMissingClosingBrace() {
		String pattern = "Value: {a";
		String result = StringHelper.format(pattern, Map.of("a", 1));
		assertEquals("Value: {a", result);
	}

	@Test
	public void shouldReplaceProps1() {
		String pattern = "bla ${foo}";
		String result = StringHelper.format(pattern, Map.of("foo", "bar"));
		assertEquals("bla bar", result);
	}

	@Test
	public void shouldReplaceProps2() {
		String pattern = "${foo} bla ";
		String result = StringHelper.format(pattern, Map.of("foo", "bar"));
		assertEquals("bar bla ", result);
	}

	@Test
	public void shouldReplaceProps3() {
		String pattern = "bla ${foo} ";
		String result = StringHelper.format(pattern, Map.of("foo", "bar"));
		assertEquals("bla bar ", result);
	}

	@Test
	public void shouldReplaceProps4() {
		String pattern = "bla${foo}abr";
		String result = StringHelper.format(pattern, Map.of("foo", "bar"));
		assertEquals("blabarabr", result);
	}

	@Test
	public void shouldReplaceProps5() {
		String pattern = "bla '${foo}' ";
		String result = StringHelper.format(pattern, Map.of("foo", "bar"));
		assertEquals("bla 'bar' ", result);
	}

	@Test
	public void shouldReplaceProps6() {
		String pattern = "${foo}bla ${foo} ";
		String result = StringHelper.format(pattern, Map.of("foo", "bar"));
		assertEquals("barbla bar ", result);
	}

	@Test
	public void shouldReplaceProps7() {
		String pattern = "${foo}bla ${food} ";
		String result = StringHelper.format(pattern, Map.of("foo", "bar", "food", "foofoo"));
		assertEquals("barbla foofoo ", result);
	}

	@Test
	public void shouldReplaceProps8() {
		String pattern = "foo";
		String result = StringHelper.format(pattern, Map.of("foo", "bar"));
		assertEquals("foo", result);
	}

	@Test
	public void shouldReplaceProps9() {
		String pattern = "${foo}bla ${food} ";
		String result = StringHelper.format(pattern, Map.of("foo", "bar", "food", "foofoo"));
		assertEquals("barbla foofoo ", result);
	}

	@Test
	public void shouldReplaceProps10() {
		String pattern = "${foo},bla ${food}. ";
		String result = StringHelper.format(pattern, Map.of("foo", "bar", "food", "foofoo"));
		assertEquals("bar,bla foofoo. ", result);
	}
}
