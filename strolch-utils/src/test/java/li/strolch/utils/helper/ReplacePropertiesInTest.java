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

package li.strolch.utils.helper;

import org.junit.Test;

import java.util.Properties;

import static org.junit.Assert.assertEquals;

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

		String result = StringHelper.replacePropertiesIn(properties, "%", expr);

		assertEquals(expected, result);
	}

	@Test
	public void shouldReplaceProps10() {

		String expr = "%{foo},bla %{food}. ";
		String expected = "bar,bla foofoo. ";

		Properties properties = new Properties();
		properties.setProperty("foo", "bar");
		properties.setProperty("food", "foofoo");

		String result = StringHelper.replacePropertiesIn(properties, "%", expr);

		assertEquals(expected, result);
	}
}
