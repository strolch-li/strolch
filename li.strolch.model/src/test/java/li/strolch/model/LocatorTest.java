/*
 * Copyright 2015 Robert von Burg <eitch@eitchnet.ch>
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
package li.strolch.model;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertTrue;

import java.util.Arrays;
import java.util.List;

import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class LocatorTest {

	@Rule
	public ExpectedException expected = ExpectedException.none();

	@Test
	public void shouldParse1() {
		List<String> paths = Arrays.asList("Resource", "MyType", "@myObj");
		assertEquals(paths, Locator.valueOf("Resource/MyType/@myObj").getPathElements());
	}

	@Test
	public void shouldParse2() {
		List<String> paths = Arrays.asList("Resource", "MyType");
		assertEquals(paths, Locator.valueOf("Resource/MyType/").getPathElements());
	}

	@Test
	public void shouldEqual1() {
		assertEquals(Locator.valueOf("Resource/MyType/"), Locator.valueOf("Resource/MyType"));
	}

	@Test
	public void shouldEqual2() {
		assertEquals(Locator.valueOf("Resource/MyType/@myObj"), Locator.valueOf("Resource/MyType/@myObj"));
	}

	@Test
	public void shouldNotEqual1() {
		assertNotEquals(Locator.valueOf("Resource/MyType"), Locator.valueOf("Resource/MyType/@myObj"));
	}

	@Test
	public void shouldNotEqual2() {
		assertNotEquals(Locator.valueOf("Resource/MyType/fdfg"), Locator.valueOf("Resource/MyType/@myObj"));
	}

	@Test
	public void shouldBeChild1() {
		Locator parent = Locator.valueOf("Resource/MyType/@myObj");
		Locator child = Locator.valueOf("Resource/MyType/@myObj/Bags/@bag1/Parameters/@param1");
		assertTrue(child.isChildOf(parent));
	}

	@Test
	public void shouldBeChild2() {
		Locator parent = Locator.valueOf("Resource/MyType/@myObj");
		Locator child = Locator.valueOf("Resource/MyType/@myObj/Bags/");
		assertTrue(child.isChildOf(parent));
	}

	@Test
	public void shouldNotBeChild1() {
		Locator parent = Locator.valueOf("Resource/MyType/@myObj");
		Locator child = Locator.valueOf("Resource/OtherType/@myObj/Bags/");
		assertFalse(child.isChildOf(parent));
	}

	@Test
	public void shouldNotBeChild2() {
		Locator parent = Locator.valueOf("Resource/MyType/@myObj");
		Locator child = Locator.valueOf("Resource/MyType/@myObj");
		assertFalse(child.isChildOf(parent));
	}
}
