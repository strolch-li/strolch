package li.strolch.utils.collections;

import org.junit.Before;
import org.junit.Test;

import java.util.Set;

import static org.junit.Assert.*;

public class ImmutableMapOfSetsTest {

	private MapOfSets<String, Integer> mapOfSets;

	@Before
	public void setUp() {
		MapOfSets<String, Integer> mapOfSets = new MapOfSets<>();
		mapOfSets.addSet("Key1", Set.of(1, 2, 3));
		mapOfSets.addSet("Key2", Set.of(4, 5, 6));
		this.mapOfSets = MapOfSets.copyOf(mapOfSets);
	}

	@Test(expected = UnsupportedOperationException.class)
	public void testAddAllWhenImmutable_ShouldThrowException() {
		MapOfSets<String, Integer> otherMap = new MapOfSets<>();
		otherMap.addSet("Key1", Set.of(1, 2, 3));
		this.mapOfSets.addAll(otherMap);
	}

	@Test(expected = UnsupportedOperationException.class)
	public void testAddElementWhenImmutable_ShouldThrowException() {
		this.mapOfSets.addElement("Key1", 1);
	}

	@Test(expected = UnsupportedOperationException.class)
	public void testAddListWhenImmutable_ShouldThrowException() {
		this.mapOfSets.addSet("Key1", Set.of(1, 2, 3));
	}

	@Test(expected = UnsupportedOperationException.class)
	public void testRemoveElementWhenImmutable_ShouldThrowException() {
		this.mapOfSets.removeElement("Key1", 1);
	}

	@Test(expected = UnsupportedOperationException.class)
	public void testRemoveListWhenImmutable_ShouldThrowException() {
		this.mapOfSets.removeSet("Key1");
	}

	@Test(expected = UnsupportedOperationException.class)
	public void testClearWhenImmutable_ShouldThrowException() {
		this.mapOfSets.clear();
	}

	@Test(expected = UnsupportedOperationException.class)
	public void testComputeIfAbsentWhenImmutable_ShouldThrowException() {
		this.mapOfSets.computeIfAbsent("Key1", _ -> Set.of(1, 2, 3));
	}

	@Test
	public void testAddAllWhenMutable_ShouldAddAll() {
		MapOfSets<String, Integer> otherMap = new MapOfSets<>();
		otherMap.addSet("Key3", Set.of(7, 8, 9));

		MapOfSets<String, Integer> mapOfSets = new MapOfSets<>(this.mapOfSets);
		assertFalse(mapOfSets.containsSet("Key3"));

		mapOfSets.addAll(otherMap);

		assertTrue(mapOfSets.containsSet("Key3"));
		assertEquals(Set.of(7, 8, 9), mapOfSets.getSet("Key3"));
	}
}