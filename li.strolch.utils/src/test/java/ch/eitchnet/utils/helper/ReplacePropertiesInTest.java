package ch.eitchnet.utils.helper;

import static org.junit.Assert.assertEquals;

import java.util.Properties;

import org.junit.Test;

public class ReplacePropertiesInTest {

	@Test
	public void shouldReplaceProps1() {

		String expr = "bla ${foo}";
		String expected = "bla bar";

		Properties properties = new Properties();
		properties.setProperty("foo", "bar");

		String result = StringHelper.replacePropertiesIn(properties, expr);

		assertEquals(expected, result);
	}

	@Test
	public void shouldReplaceProps2() {

		String expr = "${foo} bla ";
		String expected = "bar bla ";

		Properties properties = new Properties();
		properties.setProperty("foo", "bar");

		String result = StringHelper.replacePropertiesIn(properties, expr);

		assertEquals(expected, result);
	}

	@Test
	public void shouldReplaceProps3() {

		String expr = "bla ${foo} ";
		String expected = "bla bar ";

		Properties properties = new Properties();
		properties.setProperty("foo", "bar");

		String result = StringHelper.replacePropertiesIn(properties, expr);

		assertEquals(expected, result);
	}

	@Test
	public void shouldReplaceProps4() {

		String expr = "bla${foo}abr";
		String expected = "blabarabr";

		Properties properties = new Properties();
		properties.setProperty("foo", "bar");

		String result = StringHelper.replacePropertiesIn(properties, expr);

		assertEquals(expected, result);
	}

	@Test
	public void shouldReplaceProps5() {

		String expr = "bla '${foo}' ";
		String expected = "bla 'bar' ";

		Properties properties = new Properties();
		properties.setProperty("foo", "bar");

		String result = StringHelper.replacePropertiesIn(properties, expr);

		assertEquals(expected, result);
	}

	@Test
	public void shouldReplaceProps6() {

		String expr = "${foo}bla ${foo} ";
		String expected = "barbla bar ";

		Properties properties = new Properties();
		properties.setProperty("foo", "bar");

		String result = StringHelper.replacePropertiesIn(properties, expr);

		assertEquals(expected, result);
	}

	@Test
	public void shouldReplaceProps7() {

		String expr = "${foo}bla ${food} ";
		String expected = "barbla foofoo ";

		Properties properties = new Properties();
		properties.setProperty("foo", "bar");
		properties.setProperty("food", "foofoo");

		String result = StringHelper.replacePropertiesIn(properties, expr);

		assertEquals(expected, result);
	}

	@Test
	public void shouldReplaceProps8() {

		String expr = "foo";
		String expected = "foo";

		Properties properties = new Properties();
		properties.setProperty("foo", "bar");

		String result = StringHelper.replacePropertiesIn(properties, expr);

		assertEquals(expected, result);
	}

	@Test
	public void shouldReplaceProps9() {

		String expr = "%{foo}bla %{food} ";
		String expected = "barbla foofoo ";

		Properties properties = new Properties();
		properties.setProperty("foo", "bar");
		properties.setProperty("food", "foofoo");

		String result = StringHelper.replacePropertiesIn(properties, '%', expr);

		assertEquals(expected, result);
	}
}
