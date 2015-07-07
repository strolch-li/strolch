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
package ch.eitchnet.utils.dbc;

import java.io.File;
import java.text.MessageFormat;

import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import ch.eitchnet.utils.dbc.DBC.DbcException;

/**
 * The class <code>DBCTest</code> contains tests for the class <code>{@link DBC}</code>.
 * 
 * @generatedBy CodePro at 2/2/14 8:13 PM
 * @author Robert von Burg <eitch@eitchnet.ch>
 * @version $Revision: 1.0 $
 */
@SuppressWarnings("nls")
public class DBCTest {

	@Rule
	public ExpectedException exception = ExpectedException.none();

	/**
	 * Run the void assertEquals(String,Object,Object) method test.
	 * 
	 * @throws Exception
	 * 
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
	 * 
	 * @generatedBy CodePro at 2/2/14 8:13 PM
	 */
	@Test
	public void testAssertEquals_2() throws Exception {
		this.exception.expect(DbcException.class);
		Object value1 = new Object();
		Object value2 = new Object();
		String msg = MessageFormat.format("{0}: {1} != {2}", "", value1, value2);
		this.exception.expectMessage(msg);

		DBC.PRE.assertEquals(msg, value1, value2);

		// add additional test code here
		// An unexpected exception was thrown in user code while executing this test:
		//    ch.eitchnet.utils.DBC.PRE.DBC$DbcException: Values are not equal: 
		//       at ch.eitchnet.utils.DBC.PRE.DBC.PRE.assertEquals(DBC.PRE.java:39)
	}

	/**
	 * Run the void assertEquals(String,Object,Object) method test.
	 * 
	 * @throws Exception
	 * 
	 * @generatedBy CodePro at 2/2/14 8:13 PM
	 */
	@Test
	public void testAssertEquals_3() throws Exception {
		this.exception.expect(DbcException.class);

		Object value1 = null;
		Object value2 = new Object();

		String msg = MessageFormat.format("{0}: {1} != {2}", "", value1, value2);
		this.exception.expectMessage(msg);

		DBC.PRE.assertEquals(msg, value1, value2);

		// add additional test code here
	}

	/**
	 * Run the void assertEquals(String,Object,Object) method test.
	 * 
	 * @throws Exception
	 * 
	 * @generatedBy CodePro at 2/2/14 8:13 PM
	 */
	@Test
	public void testAssertEquals_4() throws Exception {
		this.exception.expect(DbcException.class);

		Object value1 = new Object();
		Object value2 = null;

		String msg = MessageFormat.format("{0}: {1} != {2}", "", value1, value2);
		this.exception.expectMessage(msg);

		DBC.PRE.assertEquals(msg, value1, value2);

		// add additional test code here
	}

	/**
	 * Run the void assertEquals(String,Object,Object) method test.
	 * 
	 * @throws Exception
	 * 
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
		//    ch.eitchnet.utils.DBC.PRE.DBC$DbcException: Values are not equal: 
		//       at ch.eitchnet.utils.DBC.PRE.DBC.PRE.assertEquals(DBC.PRE.java:39)
	}

	/**
	 * Run the void assertEquals(String,Object,Object) method test.
	 * 
	 * @throws Exception
	 * 
	 * @generatedBy CodePro at 2/2/14 8:13 PM
	 */
	@Test
	public void testAssertNotEquals_1() throws Exception {
		this.exception.expect(DbcException.class);

		String msg = "";
		Object value1 = null;
		Object value2 = null;

		String ex = "{0}: {1} == {2}";
		ex = MessageFormat.format(ex, msg, value1, value2);
		this.exception.expectMessage(ex);

		DBC.PRE.assertNotEquals(msg, value1, value2);
	}

	/**
	 * Run the void assertEquals(String,Object,Object) method test.
	 * 
	 * @throws Exception
	 * 
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
	 * 
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
	 * 
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
	 * 
	 * @generatedBy CodePro at 2/2/14 8:13 PM
	 */
	@Test
	public void testAssertNotEquals_5() throws Exception {
		this.exception.expect(DbcException.class);

		String msg = "";
		Object value1 = "bla";
		Object value2 = "bla";

		String ex = "{0}: {1} == {2}";
		ex = MessageFormat.format(ex, msg, value1, value2);
		this.exception.expectMessage(ex);

		DBC.PRE.assertNotEquals(msg, value1, value2);
	}

	/**
	 * Run the void assertExists(String,File) method test.
	 * 
	 * @throws Exception
	 * 
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
	 * 
	 * @generatedBy CodePro at 2/2/14 8:13 PM
	 */
	@Test
	public void testAssertExists_2() throws Exception {
		this.exception.expect(DbcException.class);
		this.exception.expectMessage("Illegal situation as file (srcc) does not exist:");

		String msg = "";
		File file = new File("srcc");

		DBC.PRE.assertExists(msg, file);

		// add additional test code here
		// An unexpected exception was thrown in user code while executing this test:
		//    ch.eitchnet.utils.DBC.PRE.DBC$DbcException: Illegal situation as file () does not exist: 
		//       at ch.eitchnet.utils.DBC.PRE.DBC.PRE.assertExists(DBC.PRE.java:95)
	}

	/**
	 * Run the void assertFalse(String,boolean) method test.
	 * 
	 * @throws Exception
	 * 
	 * @generatedBy CodePro at 2/2/14 8:13 PM
	 */
	@Test
	public void testAssertFalse_1() throws Exception {
		this.exception.expect(DbcException.class);
		this.exception.expectMessage("Expected false, but was true: ");

		String msg = "";
		boolean value = true;

		DBC.PRE.assertFalse(msg, value);
	}

	/**
	 * Run the void assertFalse(String,boolean) method test.
	 * 
	 * @throws Exception
	 * 
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
	 * 
	 * @generatedBy CodePro at 2/2/14 8:13 PM
	 */
	@Test
	public void testAssertNotEmpty_1() throws Exception {
		this.exception.expect(DbcException.class);
		this.exception.expectMessage("Illegal empty value: ");

		String msg = "Illegal empty value: ";
		String value = "";

		DBC.PRE.assertNotEmpty(msg, value);
	}

	/**
	 * Run the void assertNotEmpty(String,String) method test.
	 * 
	 * @throws Exception
	 * 
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
	 * 
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
	 * 
	 * @generatedBy CodePro at 2/2/14 8:13 PM
	 */
	@Test
	public void testAssertNotExists_2() throws Exception {
		this.exception.expect(DbcException.class);
		this.exception.expectMessage("Illegal situation as file (src) exists: ");

		String msg = "";
		File file = new File("src");

		DBC.PRE.assertNotExists(msg, file);

		// add additional test code here
	}

	/**
	 * Run the void assertNotNull(String,Object) method test.
	 * 
	 * @throws Exception
	 * 
	 * @generatedBy CodePro at 2/2/14 8:13 PM
	 */
	@Test
	public void testAssertNotNull_1() throws Exception {
		this.exception.expect(DbcException.class);

		String msg = "";
		Object value = null;

		String ex = "{0}: Illegal null value";
		ex = MessageFormat.format(ex, msg, value);
		this.exception.expectMessage(ex);

		DBC.PRE.assertNotNull(msg, value);
	}

	/**
	 * Run the void assertNotNull(String,Object) method test.
	 * 
	 * @throws Exception
	 * 
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
	 * 
	 * @generatedBy CodePro at 2/2/14 8:13 PM
	 */
	@Test
	public void testAssertNull_1() throws Exception {
		this.exception.expect(DbcException.class);

		Object value = new Object();

		String msg = MessageFormat.format("{0}: {1} != null", "", value);
		this.exception.expectMessage(msg);

		DBC.PRE.assertNull(msg, value);
	}

	/**
	 * Run the void assertNull(String,Object) method test.
	 * 
	 * @throws Exception
	 * 
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
	 * 
	 * @generatedBy CodePro at 2/2/14 8:13 PM
	 */
	@Test
	public void testAssertTrue_1() throws Exception {
		this.exception.expect(DbcException.class);
		this.exception.expectMessage("Expected true, but was false: ");

		String msg = "";
		boolean value = false;

		DBC.PRE.assertTrue(msg, value);

		// add additional test code here
		// An unexpected exception was thrown in user code while executing this test:
		//    ch.eitchnet.utils.DBC.PRE.DBC$DbcException: Expected true, but was false: 
		//       at ch.eitchnet.utils.DBC.PRE.DBC.PRE.assertTrue(DBC.PRE.java:47)
	}

	/**
	 * Run the void assertTrue(String,boolean) method test.
	 * 
	 * @throws Exception
	 * 
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