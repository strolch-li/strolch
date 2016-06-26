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
package li.strolch.utils.collections;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.Map;

import org.junit.Before;
import org.junit.Test;

import li.strolch.utils.collections.DefaultedHashMap;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class DefaultedHashMapTest {

	private Map<String, String> map;

	@Before
	public void setUp() {
		this.map = new DefaultedHashMap<>("foobar");
		this.map.put("foo", "foofoo");
	}

	@Test
	public void shouldReturnMappedValue() {
		assertTrue(this.map.containsKey("foo"));
		assertEquals("foofoo", this.map.get("foo"));
	}

	@Test
	public void shouldReturnDefaultValue() {
		assertFalse(this.map.containsKey("bar"));
		assertEquals("foobar", this.map.get("bar"));
	}
}
