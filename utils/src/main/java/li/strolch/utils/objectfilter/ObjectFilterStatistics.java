package li.strolch.utils.objectfilter;

import java.util.HashMap;
import java.util.Map;

public record ObjectFilterStatistics(Map<String, Integer> added, Map<String, Integer> updated,
									 Map<String, Integer> removed) {

	public ObjectFilterStatistics merge(ObjectFilterStatistics other) {
		Map<String, Integer> added = new HashMap<>(added());
		Map<String, Integer> updated = new HashMap<>(updated());
		Map<String, Integer> removed = new HashMap<>(removed());
		other.added().forEach((key, count) -> addCountKey(added, key, count));
		other.updated().forEach((key, count) -> addCountKey(updated, key, count));
		other.removed().forEach((key, count) -> addCountKey(removed, key, count));
		return new ObjectFilterStatistics(added, updated, removed);
	}

	static void addCountKey(Map<String, Integer> map, String key, int count) {
		if (map.containsKey(key))
			map.put(key, map.get(key) + count);
		else
			map.put(key, count);
	}

	@Override
	public String toString() {
		return "ObjectFilterStatistics{" + "added=" + added + ", updated=" + updated + ", removed=" + removed + '}';
	}
}
