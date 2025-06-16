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

import java.util.Arrays;

import static org.junit.Assert.*;

public class ImmutableMapOfListsTest {

	private MapOfLists<String, Integer> mapOfLists;

	@Before
	public void setUp() {
		MapOfLists<String, Integer> mapOfLists = new MapOfLists<>();
		mapOfLists.addList("Key1", Arrays.asList(1, 2, 3));
		mapOfLists.addList("Key2", Arrays.asList(4, 5, 6));
		this.mapOfLists = MapOfLists.copyOf(mapOfLists);
	}

	@Test(expected = UnsupportedOperationException.class)
	public void testAddAllWhenImmutable_ShouldThrowException() {
		MapOfLists<String, Integer> otherMap = new MapOfLists<>();
		otherMap.addList("Key1", Arrays.asList(1, 2, 3));
		this.mapOfLists.addAll(otherMap);
	}

	@Test(expected = UnsupportedOperationException.class)
	public void testAddElementWhenImmutable_ShouldThrowException() {
		this.mapOfLists.addElement("Key1", 1);
	}

	@Test(expected = UnsupportedOperationException.class)
	public void testAddListWhenImmutable_ShouldThrowException() {
		this.mapOfLists.addList("Key1", Arrays.asList(1, 2, 3));
	}

	@Test(expected = UnsupportedOperationException.class)
	public void testRemoveElementWhenImmutable_ShouldThrowException() {
		this.mapOfLists.removeElement("Key1", 1);
	}

	@Test(expected = UnsupportedOperationException.class)
	public void testRemoveListWhenImmutable_ShouldThrowException() {
		this.mapOfLists.removeList("Key1");
	}

	@Test(expected = UnsupportedOperationException.class)
	public void testClearWhenImmutable_ShouldThrowException() {
		this.mapOfLists.clear();
	}

	@Test(expected = UnsupportedOperationException.class)
	public void testComputeIfAbsentWhenImmutable_ShouldThrowException() {
		this.mapOfLists.computeIfAbsent("Key1", _ -> Arrays.asList(1, 2, 3));
	}

	@Test(expected = UnsupportedOperationException.class)
	public void testGetListThenAddWhenImmutable_ShouldThrowException() {
		this.mapOfLists.getList("Key1").add(2);
	}

	@Test
	public void testAddAllWhenMutable_ShouldAddAll() {
		MapOfLists<String, Integer> otherMap = new MapOfLists<>();
		otherMap.addList("Key3", Arrays.asList(7, 8, 9));

		MapOfLists<String, Integer> mapOfLists = new MapOfLists<>(this.mapOfLists);
		assertFalse(mapOfLists.containsList("Key3"));

		mapOfLists.addAll(otherMap);

		assertTrue(mapOfLists.containsList("Key3"));
		assertEquals(Arrays.asList(7, 8, 9), mapOfLists.getList("Key3"));
	}
}