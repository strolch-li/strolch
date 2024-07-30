package li.strolch.utils.collections;

import static java.util.Collections.*;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.util.*;

import org.junit.Test;

public class MapOfTest {

	@Test
	public void shouldTestMapOfLists() {

		MapOfLists<String, String> mapOfLists = new MapOfLists<>();
		mapOfLists.addElement("a", "1");
		mapOfLists.addElement("a", "2");
		mapOfLists.addElement("b", "3");
		mapOfLists.addElement("b", "4");

		List<String> list;
		list = mapOfLists.getList("a");
		assertNotNull(list);
		assertEquals(2, list.size());
		assertEquals("1", list.get(0));
		assertEquals("2", list.get(1));

		list = mapOfLists.getList("b");
		assertNotNull(list);
		assertEquals(2, list.size());
		assertEquals("3", list.get(0));
		assertEquals("4", list.get(1));

		mapOfLists.computeIfAbsent("c", s -> {
			List<String> items = new ArrayList<>();
			items.add("5");
			return items;
		});

		list = mapOfLists.getList("c");
		assertNotNull(list);
		assertEquals(1, list.size());
		assertEquals("5", list.get(0));

		list = mapOfLists.getListOrDefault("a", emptyList());
		assertNotNull(list);
		assertEquals(2, list.size());
		assertEquals("1", list.get(0));
		assertEquals("2", list.get(1));

		list = mapOfLists.getListOrDefault("d", emptyList());
		assertNotNull(list);
		assertEquals(list, emptyList());

		mapOfLists.forEach((key, items) -> {
			if (key.equals("a")) {
				assertNotNull(items);
				assertEquals(2, items.size());
				assertEquals("1", items.get(0));
				assertEquals("2", items.get(1));
			}
		});
	}

	@Test
	public void shouldTestMapOfMaps() {

		MapOfMaps<String, String, Integer> mapOfMaps = new MapOfMaps<>();
		mapOfMaps.addElement("a", "1", 1);
		mapOfMaps.addElement("a", "2", 2);
		mapOfMaps.addElement("b", "3", 3);
		mapOfMaps.addElement("b", "4", 4);

		Map<String, Integer> map;
		map = mapOfMaps.getMap("a");
		assertNotNull(map);
		assertEquals(2, map.size());
		assertEquals(Integer.valueOf(1), map.get("1"));
		assertEquals(Integer.valueOf(2), map.get("2"));

		map = mapOfMaps.getMap("b");
		assertNotNull(map);
		assertEquals(2, map.size());
		assertEquals(Integer.valueOf(3), map.get("3"));
		assertEquals(Integer.valueOf(4), map.get("4"));

		mapOfMaps.computeIfAbsent("c", s -> {
			Map<String, Integer> items = new HashMap<>();
			items.put("5", 5);
			return items;
		});

		map = mapOfMaps.getMap("c");
		assertNotNull(map);
		assertEquals(1, map.size());
		assertEquals(Integer.valueOf(5), map.get("5"));

		map = mapOfMaps.getMapOrDefault("a", emptyMap());
		assertNotNull(map);
		assertEquals(2, map.size());
		assertEquals(Integer.valueOf(1), map.get("1"));
		assertEquals(Integer.valueOf(2), map.get("2"));

		map = mapOfMaps.getMapOrDefault("d", emptyMap());
		assertNotNull(map);
		assertEquals(map, emptyMap());

		mapOfMaps.forEach((key, items) -> {
			if (key.equals("a")) {
				assertNotNull(items);
				assertEquals(2, items.size());
				assertEquals(Integer.valueOf(1), items.get("1"));
				assertEquals(Integer.valueOf(2), items.get("2"));
			}
		});
	}

	@Test
	public void shouldTestMapOfSets() {

		MapOfSets<String, String> mapOfSets = new MapOfSets<>();
		mapOfSets.addElement("a", "1");
		mapOfSets.addElement("a", "2");
		mapOfSets.addElement("b", "3");
		mapOfSets.addElement("b", "4");

		Set<String> set;
		set = mapOfSets.getSet("a");
		assertNotNull(set);
		assertEquals(new HashSet<>(List.of("1", "2")), set);

		set = mapOfSets.getSet("b");
		assertNotNull(set);
		assertEquals(new HashSet<>(List.of("3", "4")), set);

		mapOfSets.computeIfAbsent("c", s -> {
			Set<String> items = new HashSet<>();
			items.add("5");
			return items;
		});

		set = mapOfSets.getSet("c");
		assertNotNull(set);
		assertEquals(new HashSet<>(List.of("5")), set);

		set = mapOfSets.getSetOrDefault("a", emptySet());
		assertNotNull(set);
		assertEquals(new HashSet<>(List.of("1", "2")), set);

		set = mapOfSets.getSetOrDefault("d", emptySet());
		assertNotNull(set);
		assertEquals(set, emptySet());

		mapOfSets.forEach((key, items) -> {
			if (key.equals("a")) {
				assertNotNull(items);
				assertEquals(new HashSet<>(List.of("1", "2")), items);
			}
		});
	}
}
