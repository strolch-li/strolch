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
 * @generatedBy CodePro at 2/2/14 8:13 PM
 */
@SuppressWarnings("nls")
public class DBCTest {

	/**
	 * Run the void assertEquals(String,Object,Object) method test.
	 *
	 * @throws Exception
	 * 		if something goes wrong if something goes wrong
	 * @generatedBy CodePro at 2/2/14 8:13 PM
	 */
	@Test
	public void testAssertEquals_1() throws Exception {
		String msg = "";
		Object value1 = null;
		Object value2 = null;

		DBC.PRE.assertEquals(msg, value1, value2);

		// add additional test code here
	}

	/**
	 * Run the void assertEquals(String,Object,Object) method test.
	 *
	 * @throws Exception
	 * 		if something goes wrong if something goes wrong
	 * @generatedBy CodePro at 2/2/14 8:13 PM
	 */
	@Test
	public void testAssertEquals_2() throws Exception {
		Object value1 = new Object();
		Object value2 = new Object();
		String msg = MessageFormat.format("{0}: {1} != {2}", "", value1, value2);

		DbcException dbcException = Assert.assertThrows(DbcException.class, () -> {
			DBC.PRE.assertEquals(msg, value1, value2);
		});

		assertThat(dbcException.getMessage(), containsString(msg));
	}

	/**
	 * Run the void assertEquals(String,Object,Object) method test.
	 *
	 * @throws Exception
	 * 		if something goes wrong if something goes wrong
	 * @generatedBy CodePro at 2/2/14 8:13 PM
	 */
	@Test
	public void testAssertEquals_3() throws Exception {
		Object value1 = null;
		Object value2 = new Object();
		String msg = MessageFormat.format("{0}: {1} != {2}", "", value1, value2);

		DbcException dbcException = Assert.assertThrows(DbcException.class, () -> {
			DBC.PRE.assertEquals(msg, value1, value2);
		});

		assertThat(dbcException.getMessage(), containsString(msg));
	}

	/**
	 * Run the void assertEquals(String,Object,Object) method test.
	 *
	 * @throws Exception
	 * 		if something goes wrong if something goes wrong
	 * @generatedBy CodePro at 2/2/14 8:13 PM
	 */
	@Test
	public void testAssertEquals_4() throws Exception {
		Object value1 = new Object();
		Object value2 = null;

		String msg = MessageFormat.format("{0}: {1} != {2}", "", value1, value2);

		DbcException dbcException = Assert.assertThrows(DbcException.class, () -> {
			DBC.PRE.assertEquals(msg, value1, value2);
		});

		assertThat(dbcException.getMessage(), containsString(msg));
	}

	/**
	 * Run the void assertEquals(String,Object,Object) method test.
	 *
	 * @throws Exception
	 * 		if something goes wrong if something goes wrong
	 * @generatedBy CodePro at 2/2/14 8:13 PM
	 */
	@Test
	public void testAssertEquals_5() throws Exception {
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
	 *
	 * @throws Exception
	 * 		if something goes wrong if something goes wrong
	 * @generatedBy CodePro at 2/2/14 8:13 PM
	 */
	@Test
	public void testAssertNotEquals_1() throws Exception {
		String msg = "";
		Object value1 = null;
		Object value2 = null;

		String ex = "{0}: {1} == {2}";
		String expectedMsg = MessageFormat.format(ex, msg, value1, value2);

		DbcException dbcException = Assert.assertThrows(DbcException.class, () -> {
			DBC.PRE.assertNotEquals(expectedMsg, value1, value2);
		});

		assertThat(dbcException.getMessage(), containsString(msg));
	}

	/**
	 * Run the void assertEquals(String,Object,Object) method test.
	 *
	 * @throws Exception
	 * 		if something goes wrong if something goes wrong
	 * @generatedBy CodePro at 2/2/14 8:13 PM
	 */
	@Test
	public void testAssertNotEquals_2() throws Exception {
		String msg = "";
		Object value1 = new Object();
		Object value2 = new Object();

		DBC.PRE.assertNotEquals(msg, value1, value2);
	}

	/**
	 * Run the void assertEquals(String,Object,Object) method test.
	 *
	 * @throws Exception
	 * 		if something goes wrong if something goes wrong
	 * @generatedBy CodePro at 2/2/14 8:13 PM
	 */
	@Test
	public void testAssertNotEquals_3() throws Exception {
		String msg = "";
		Object value1 = null;
		Object value2 = new Object();

		DBC.PRE.assertNotEquals(msg, value1, value2);
	}

	/**
	 * Run the void assertEquals(String,Object,Object) method test.
	 *
	 * @throws Exception
	 * 		if something goes wrong if something goes wrong
	 * @generatedBy CodePro at 2/2/14 8:13 PM
	 */
	@Test
	public void testAssertNotEquals_4() throws Exception {
		String msg = "";
		Object value1 = new Object();
		Object value2 = null;

		DBC.PRE.assertNotEquals(msg, value1, value2);
	}

	/**
	 * Run the void assertEquals(String,Object,Object) method test.
	 *
	 * @throws Exception
	 * 		if something goes wrong if something goes wrong
	 * @generatedBy CodePro at 2/2/14 8:13 PM
	 */
	@Test
	public void testAssertNotEquals_5() throws Exception {
		String msg = "";
		Object value1 = "bla";
		Object value2 = "bla";

		String ex = "{0}: {1} == {2}";
		String expectedMsg = MessageFormat.format(ex, msg, value1, value2);

		DbcException dbcException = Assert.assertThrows(DbcException.class, () -> {
			DBC.PRE.assertNotEquals(expectedMsg, value1, value2);
		});

		assertThat(dbcException.getMessage(), containsString(msg));
	}

	/**
	 * Run the void assertExists(String,File) method test.
	 *
	 * @throws Exception
	 * 		if something goes wrong if something goes wrong
	 * @generatedBy CodePro at 2/2/14 8:13 PM
	 */
	@Test
	public void testAssertExists_1() throws Exception {
		String msg = "";
		File file = new File("src");

		DBC.PRE.assertExists(msg, file);
	}

	/**
	 * Run the void assertExists(String,File) method test.
	 *
	 * @throws Exception
	 * 		if something goes wrong if something goes wrong
	 * @generatedBy CodePro at 2/2/14 8:13 PM
	 */
	@Test
	public void testAssertExists_2() throws Exception {
		String msg = "";
		File file = new File("srcc");

		DbcException dbcException = Assert.assertThrows(DbcException.class, () -> {
			DBC.PRE.assertExists(msg, file);
		});

		assertThat(dbcException.getMessage(), containsString("Illegal situation as file (srcc) does not exist:"));
	}

	/**
	 * Run the void assertFalse(String,boolean) method test.
	 *
	 * @throws Exception
	 * 		if something goes wrong if something goes wrong
	 * @generatedBy CodePro at 2/2/14 8:13 PM
	 */
	@Test
	public void testAssertFalse_1() throws Exception {
		String msg = "";
		boolean value = true;

		DbcException dbcException = Assert.assertThrows(DbcException.class, () -> {
			DBC.PRE.assertFalse(msg, value);
		});

		assertThat(dbcException.getMessage(), containsString("Expected false, but was true: "));
	}

	/**
	 * Run the void assertFalse(String,boolean) method test.
	 *
	 * @throws Exception
	 * 		if something goes wrong
	 * @generatedBy CodePro at 2/2/14 8:13 PM
	 */
	@Test
	public void testAssertFalse_2() throws Exception {
		String msg = "";
		boolean value = false;

		DBC.PRE.assertFalse(msg, value);

		// add additional test code here
	}

	/**
	 * Run the void assertNotEmpty(String,String) method test.
	 *
	 * @throws Exception
	 * 		if something goes wrong
	 * @generatedBy CodePro at 2/2/14 8:13 PM
	 */
	@Test
	public void testAssertNotEmpty_1() throws Exception {
		String msg = "Illegal empty value: ";
		String value = "";

		DbcException dbcException = Assert.assertThrows(DbcException.class, () -> {
			DBC.PRE.assertNotEmpty(msg, value);
		});

		assertThat(dbcException.getMessage(), containsString("Illegal empty value: "));
	}

	/**
	 * Run the void assertNotEmpty(String,String) method test.
	 *
	 * @throws Exception
	 * 		if something goes wrong
	 * @generatedBy CodePro at 2/2/14 8:13 PM
	 */
	@Test
	public void testAssertNotEmpty_2() throws Exception {
		String msg = "";
		String value = "a";

		DBC.PRE.assertNotEmpty(msg, value);
	}

	/**
	 * Run the void assertNotExists(String,File) method test.
	 *
	 * @throws Exception
	 * 		if something goes wrong
	 * @generatedBy CodePro at 2/2/14 8:13 PM
	 */
	@Test
	public void testAssertNotExists_1() throws Exception {
		String msg = "";
		File file = new File("srcc");

		DBC.PRE.assertNotExists(msg, file);

		// add additional test code here
	}

	/**
	 * Run the void assertNotExists(String,File) method test.
	 *
	 * @throws Exception
	 * 		if something goes wrong
	 * @generatedBy CodePro at 2/2/14 8:13 PM
	 */
	@Test
	public void testAssertNotExists_2() throws Exception {
		String msg = "";
		File file = new File("src");

		DbcException dbcException = Assert.assertThrows(DbcException.class, () -> {
			DBC.PRE.assertNotExists(msg, file);
		});

		assertEquals("Illegal situation as file (src) exists: ", dbcException.getMessage());
	}

	/**
	 * Run the void assertNotNull(String,Object) method test.
	 *
	 * @throws Exception
	 * 		if something goes wrong
	 * @generatedBy CodePro at 2/2/14 8:13 PM
	 */
	@Test
	public void testAssertNotNull_1() throws Exception {
		String msg = "";
		Object value = null;

		String ex = "{0}: Illegal null value";
		String expectedMsg = MessageFormat.format(ex, msg, value);

		DbcException dbcException = Assert.assertThrows(DbcException.class, () -> {
			DBC.PRE.assertNotNull(expectedMsg, value);
		});

		assertThat(dbcException.getMessage(), containsString(msg));
	}

	/**
	 * Run the void assertNotNull(String,Object) method test.
	 *
	 * @throws Exception
	 * 		if something goes wrong
	 * @generatedBy CodePro at 2/2/14 8:13 PM
	 */
	@Test
	public void testAssertNotNull_2() throws Exception {
		String msg = "";
		Object value = new Object();

		DBC.PRE.assertNotNull(msg, value);

		// add additional test code here
	}

	/**
	 * Run the void assertNull(String,Object) method test.
	 *
	 * @throws Exception
	 * 		if something goes wrong
	 * @generatedBy CodePro at 2/2/14 8:13 PM
	 */
	@Test
	public void testAssertNull_1() throws Exception {
		Object value = new Object();

		String msg = MessageFormat.format("{0}: {1} != null", "", value);

		DbcException dbcException = Assert.assertThrows(DbcException.class, () -> {
			DBC.PRE.assertNull(msg, value);
		});

		assertThat(dbcException.getMessage(), containsString(msg));
	}

	/**
	 * Run the void assertNull(String,Object) method test.
	 *
	 * @throws Exception
	 * 		if something goes wrong
	 * @generatedBy CodePro at 2/2/14 8:13 PM
	 */
	@Test
	public void testAssertNull_2() throws Exception {
		String msg = "";
		Object value = null;

		DBC.PRE.assertNull(msg, value);

		// add additional test code here
	}

	/**
	 * Run the void assertTrue(String,boolean) method test.
	 *
	 * @throws Exception
	 * 		if something goes wrong
	 * @generatedBy CodePro at 2/2/14 8:13 PM
	 */
	@Test
	public void testAssertTrue_1() throws Exception {
		String msg = "";
		boolean value = false;

		DbcException dbcException = Assert.assertThrows(DbcException.class, () -> {
			DBC.PRE.assertTrue(msg, value);
		});

		assertEquals("Expected true, but was false: ", dbcException.getMessage());
	}

	/**
	 * Run the void assertTrue(String,boolean) method test.
	 *
	 * @throws Exception
	 * 		if something goes wrong
	 * @generatedBy CodePro at 2/2/14 8:13 PM
	 */
	@Test
	public void testAssertTrue_2() throws Exception {
		String msg = "";
		boolean value = true;

		DBC.PRE.assertTrue(msg, value);

		// add additional test code here
	}
}