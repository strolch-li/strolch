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