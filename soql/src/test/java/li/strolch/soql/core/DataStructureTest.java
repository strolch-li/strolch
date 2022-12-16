package li.strolch.soql.core;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import li.strolch.model.StrolchRootElement;
import org.junit.Before;
import org.junit.Test;

public class DataStructureTest extends BaseTest {

	Map<String, List<StrolchRootElement>> structure;

	@Before
	public void init() {
		structure = new LinkedHashMap<>();
		structure.put("r", getTestRessources(6));
		structure.put("o", getTestOrders(3));
		structure.put("a", getTestActivities(4));
		// System.out.println(structure);
	}

	@Test
	public void buildProduct() {

		// it's ugly indexing stuff, so here we go
		final Object[] keys = structure.keySet().toArray(); // the nicknames of the entities
		final int numberOfKeys = keys.length;

		// get an overview of how many elements we have to take into account
		final int[] numberOfEntities = new int[numberOfKeys];
		for (int keyIndex = 0; keyIndex < numberOfKeys; keyIndex++) {
			numberOfEntities[keyIndex] = structure.get(keys[keyIndex]).size();
		}

		// build cartesian product
		final List<List<StrolchRootElement>> cartesianProduct = new ArrayList<>();
		final IndexPointer indexPointer = new IndexPointer(numberOfEntities);
		while (indexPointer.hasNext()) {

			final List<StrolchRootElement> row = new ArrayList<>();
			cartesianProduct.add(row);

			int[] pointer = indexPointer.next();

			// fasten your seat belts, here we go
			for (int keyIndex = 0; keyIndex < numberOfKeys; keyIndex++) {
				final Object key = keys[keyIndex];
				final List<StrolchRootElement> elements = structure.get(key);
				final StrolchRootElement element = elements.get(pointer[keyIndex]);
				row.add(element);
			}
		}
	}

}
