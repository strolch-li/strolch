/*
 * Copyright (c) 2025 Robert von Burg <eitch@eitchnet.ch>
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

import org.junit.Before;
import org.junit.Test;

import java.util.Map;

import static org.junit.Assert.*;

public class ImmutableMapOfMapsTest {

	private MapOfMaps<String, String, Integer> mapOfMaps;

	@Before
	public void setUp() {
		MapOfMaps<String, String, Integer> mapOfMaps = new MapOfMaps<>();
		mapOfMaps.addMap("Key1", Map.of("fruits", 1));
		mapOfMaps.addMap("Key2", Map.of("fruits", 2));
		this.mapOfMaps = MapOfMaps.copyOf(mapOfMaps);
	}

	@Test(expected = UnsupportedOperationException.class)
	public void testPutAllWhenImmutable_ShouldThrowException() {
		MapOfMaps<String, String, Integer> otherMap = new MapOfMaps<>();
		otherMap.addMap("Key3", Map.of("fruits", 2));
		this.mapOfMaps.putAll(otherMap);
	}

	@Test(expected = UnsupportedOperationException.class)
	public void testAddElementWhenImmutable_ShouldThrowException() {
		this.mapOfMaps.addElement("Key1", "fruits", 1);
	}

	@Test(expected = UnsupportedOperationException.class)
	public void testAddListWhenImmutable_ShouldThrowException() {
		this.mapOfMaps.addMap("Key1", Map.of("fruits", 2));
	}

	@Test(expected = UnsupportedOperationException.class)
	public void testRemoveElementWhenImmutable_ShouldThrowException() {
		this.mapOfMaps.removeElement("Key1", "fruits");
	}

	@Test(expected = UnsupportedOperationException.class)
	public void testRemoveListWhenImmutable_ShouldThrowException() {
		this.mapOfMaps.removeMap("Key1");
	}

	@Test(expected = UnsupportedOperationException.class)
	public void testClearWhenImmutable_ShouldThrowException() {
		this.mapOfMaps.clear();
	}

	@Test(expected = UnsupportedOperationException.class)
	public void testComputeIfAbsentWhenImmutable_ShouldThrowException() {
		this.mapOfMaps.computeIfAbsent("Key1", _ -> Map.of("fruits", 2));
	}

	@Test(expected = UnsupportedOperationException.class)
	public void testGeMapThenPutWhenImmutable_ShouldThrowException() {
		this.mapOfMaps.getMap("Key1").put("fruits", 2);
	}

	@Test
	public void testPutAllWhenMutable_ShouldPutAll() {
		MapOfMaps<String, String, Integer> otherMap = new MapOfMaps<>();
		otherMap.addMap("Key3", Map.of("fruits", 2));

		MapOfMaps<String, String, Integer> mapOfMaps = new MapOfMaps<>(this.mapOfMaps);
		assertFalse(mapOfMaps.containsMap("Key3"));

		mapOfMaps.putAll(otherMap);

		assertTrue(mapOfMaps.containsMap("Key3"));
		assertEquals(Map.of("fruits", 2), mapOfMaps.getMap("Key3"));
	}
}
