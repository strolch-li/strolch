/*
 * Copyright 2014 Robert von Burg <eitch@eitchnet.ch>
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
package li.strolch.utils.dbc;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.core.StringContains.containsString;
import static org.junit.Assert.assertEquals;

import java.io.File;
import java.text.MessageFormat;

import li.strolch.utils.dbc.DBC.DbcException;
import org.junit.Assert;
import org.junit.Test;

/**
 * The class <code>DBCTest</code> contains tests for the class <code>{@link DBC}</code>.
 *
 * @author Robert von Burg &lt;eitch@eitchnet.ch&gt;
 * @version $Revision: 1.0 $
 */
@SuppressWarnings("nls")
public class DBCTest {

	/**
	 * Run the void assertEquals(String,Object,Object) method test.
	 */
	@Test
	public void testAssertEquals_1() {
		String msg = "";
		DBC.PRE.assertEquals(msg, null, null);
	}

	/**
	 * Run the void assertEquals(String,Object,Object) method test.
	 */
	@Test
	public void testAssertEquals_2() {
		Object value1 = new Object();
		Object value2 = new Object();
		String msg = MessageFormat.format("{0}: {1} != {2}", "", value1, value2);

		DbcException dbcException = Assert.assertThrows(DbcException.class,
				() -> DBC.PRE.assertEquals(msg, value1, value2));

		assertThat(dbcException.getMessage(), containsString(msg));
	}

	/**
	 * Run the void assertEquals(String,Object,Object) method test.
	 */
	@Test
	public void testAssertEquals_3() {
		Object value2 = new Object();
		String msg = MessageFormat.format("{0}: {1} != {2}", "", null, value2);

		DbcException dbcException = Assert.assertThrows(DbcException.class,
				() -> DBC.PRE.assertEquals(msg, null, value2));

		assertThat(dbcException.getMessage(), containsString(msg));
	}

	/**
	 * Run the void assertEquals(String,Object,Object) method test.
	 */
	@Test
	public void testAssertEquals_4() {
		Object value1 = new Object();

		String msg = MessageFormat.format("{0}: {1} != {2}", "", value1, null);

		DbcException dbcException = Assert.assertThrows(DbcException.class,
				() -> DBC.PRE.assertEquals(msg, value1, null));

		assertThat(dbcException.getMessage(), containsString(msg));
	}

	/**
	 * Run the void assertEquals(String,Object,Object) method test.
	 */
	@Test
	public void testAssertEquals_5() {
		String msg = "";
		Object value1 = "bla";
		Object value2 = "bla";

		DBC.PRE.assertEquals(msg, value1, value2);

		// add additional test code here
		// An unexpected exception was thrown in user code while executing this test:
		//    li.strolch.utils.DBC.PRE.DBC$DbcException: Values are not equal: 
		//       at li.strolch.utils.DBC.PRE.DBC.PRE.assertEquals(DBC.PRE.java:39)
	}

	/**
	 * Run the void assertEquals(String,Object,Object) method test.
	 */
	@Test
	public void testAssertNotEquals_1() {
		String msg = "";

		String ex = "{0}: {1} == {2}";
		String expectedMsg = MessageFormat.format(ex, msg, null, null);

		DbcException dbcException = Assert.assertThrows(DbcException.class,
				() -> DBC.PRE.assertNotEquals(expectedMsg, null, null));

		assertThat(dbcException.getMessage(), containsString(msg));
	}

	/**
	 * Run the void assertEquals(String,Object,Object) method test.
	 */
	@Test
	public void testAssertNotEquals_2() {
		String msg = "";
		Object value1 = new Object();
		Object value2 = new Object();

		DBC.PRE.assertNotEquals(msg, value1, value2);
	}

	/**
	 * Run the void assertEquals(String,Object,Object) method test.
	 */
	@Test
	public void testAssertNotEquals_3() {
		String msg = "";
		Object value2 = new Object();

		DBC.PRE.assertNotEquals(msg, null, value2);
	}

	/**
	 * Run the void assertEquals(String,Object,Object) method test.
	 */
	@Test
	public void testAssertNotEquals_4() {
		String msg = "";
		Object value1 = new Object();

		DBC.PRE.assertNotEquals(msg, value1, null);
	}

	/**
	 * Run the void assertEquals(String,Object,Object) method test.
	 */
	@Test
	public void testAssertNotEquals_5() {
		String msg = "";
		Object value1 = "bla";
		Object value2 = "bla";

		String ex = "{0}: {1} == {2}";
		String expectedMsg = MessageFormat.format(ex, msg, value1, value2);

		DbcException dbcException = Assert.assertThrows(DbcException.class,
				() -> DBC.PRE.assertNotEquals(expectedMsg, value1, value2));

		assertThat(dbcException.getMessage(), containsString(msg));
	}

	/**
	 * Run the void assertExists(String,File) method test.
	 */
	@Test
	public void testAssertExists_1() {
		String msg = "";
		File file = new File("src");

		DBC.PRE.assertExists(msg, file);
	}

	/**
	 * Run the void assertExists(String,File) method test.
	 */
	@Test
	public void testAssertExists_2() {
		String msg = "";
		File file = new File("srcc");

		DbcException dbcException = Assert.assertThrows(DbcException.class, () -> DBC.PRE.assertExists(msg, file));

		assertThat(dbcException.getMessage(), containsString("Illegal situation as file (srcc) does not exist:"));
	}

	/**
	 * Run the void assertFalse(String,boolean) method test.
	 */
	@Test
	public void testAssertFalse_1() {
		String msg = "";
		boolean value = true;

		DbcException dbcException = Assert.assertThrows(DbcException.class, () -> DBC.PRE.assertFalse(msg, value));

		assertThat(dbcException.getMessage(), containsString("Expected false, but was true: "));
	}

	/**
	 * Run the void assertFalse(String,boolean) method test.
	 */
	@Test
	public void testAssertFalse_2() {
		String msg = "";
		boolean value = false;

		DBC.PRE.assertFalse(msg, value);

		// add additional test code here
	}

	/**
	 * Run the void assertNotEmpty(String,String) method test.
	 */
	@Test
	public void testAssertNotEmpty_1() {
		String msg = "Illegal empty value: ";
		String value = "";

		DbcException dbcException = Assert.assertThrows(DbcException.class, () -> DBC.PRE.assertNotEmpty(msg, value));

		assertThat(dbcException.getMessage(), containsString("Illegal empty value: "));
	}

	/**
	 * Run the void assertNotEmpty(String,String) method test.
	 */
	@Test
	public void testAssertNotEmpty_2() {
		String msg = "";
		String value = "a";

		DBC.PRE.assertNotEmpty(msg, value);
	}

	/**
	 * Run the void assertNotExists(String,File) method test.
	 */
	@Test
	public void testAssertNotExists_1() {
		String msg = "";
		File file = new File("srcc");

		DBC.PRE.assertNotExists(msg, file);

		// add additional test code here
	}

	/**
	 * Run the void assertNotExists(String,File) method test.
	 */
	@Test
	public void testAssertNotExists_2() {
		String msg = "";
		File file = new File("src");

		DbcException dbcException = Assert.assertThrows(DbcException.class, () -> DBC.PRE.assertNotExists(msg, file));

		assertEquals("Illegal situation as file (src) exists: ", dbcException.getMessage());
	}

	/**
	 * Run the void assertNotNull(String,Object) method test.
	 */
	@Test
	public void testAssertNotNull_1() {
		String msg = "";

		String ex = "{0}: Illegal null value";
		String expectedMsg = MessageFormat.format(ex, msg, null);

		DbcException dbcException = Assert.assertThrows(DbcException.class,
				() -> DBC.PRE.assertNotNull(expectedMsg, null));

		assertThat(dbcException.getMessage(), containsString(msg));
	}

	/**
	 * Run the void assertNotNull(String,Object) method test.
	 */
	@Test
	public void testAssertNotNull_2() {
		String msg = "";
		Object value = new Object();

		DBC.PRE.assertNotNull(msg, value);

		// add additional test code here
	}

	/**
	 * Run the void assertNull(String,Object) method test.
	 */
	@Test
	public void testAssertNull_1() {
		Object value = new Object();

		String msg = MessageFormat.format("{0}: {1} != null", "", value);

		DbcException dbcException = Assert.assertThrows(DbcException.class, () -> DBC.PRE.assertNull(msg, value));

		assertThat(dbcException.getMessage(), containsString(msg));
	}

	/**
	 * Run the void assertNull(String,Object) method test.
	 */
	@Test
	public void testAssertNull_2() {
		String msg = "";

		DBC.PRE.assertNull(msg, null);

		// add additional test code here
	}

	/**
	 * Run the void assertTrue(String,boolean) method test.
	 */
	@Test
	public void testAssertTrue_1() {
		String msg = "";
		boolean value = false;

		DbcException dbcException = Assert.assertThrows(DbcException.class, () -> DBC.PRE.assertTrue(msg, value));

		assertEquals("Expected true, but was false: ", dbcException.getMessage());
	}

	/**
	 * Run the void assertTrue(String,boolean) method test.
	 */
	@Test
	public void testAssertTrue_2() {
		String msg = "";
		boolean value = true;

		DBC.PRE.assertTrue(msg, value);

		// add additional test code here
	}
}