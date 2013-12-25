package li.strolch.persistence.inmemory;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import li.strolch.model.StrolchElement;
import li.strolch.persistence.api.StrolchDao;

public class InMemoryDao<T extends StrolchElement> implements StrolchDao<T> {

	private Map<String, Map<String, T>> elementMap;

	public InMemoryDao() {
		this.elementMap = new HashMap<>();
	}

	@Override
	public Set<String> queryKeySet() {

		Set<String> keySet = new HashSet<>();
		for (String type : this.elementMap.keySet()) {
			Map<String, T> byType = this.elementMap.get(type);
			for (String id : byType.keySet()) {
				keySet.add(id);
			}
		}

		return keySet;
	}

	@Override
	public Set<String> queryKeySet(String type) {

		if (!this.elementMap.containsKey(type))
			return Collections.emptySet();

		return this.elementMap.get(type).keySet();
	}

	@Override
	public Set<String> queryTypes() {
		return this.elementMap.keySet();
	}

	@Override
	public T queryBy(String type, String id) {
		if (!this.elementMap.containsKey(type))
			return null;

		return this.elementMap.get(type).get(id);
	}

	@Override
	public List<T> queryAll() {
		List<T> elements = new ArrayList<>();
		for (String type : this.elementMap.keySet()) {
			Map<String, T> byType = this.elementMap.get(type);
			for (String id : byType.keySet()) {
				elements.add(byType.get(id));
			}
		}

		return elements;
	}

	@Override
	public List<T> queryAll(String type) {
		if (!this.elementMap.containsKey(type))
			return Collections.emptyList();

		return new ArrayList<>(this.elementMap.get(type).values());
	}

	@Override
	public void save(T element) {
		Map<String, T> byType = this.elementMap.get(element.getType());
		if (byType == null) {
			byType = new HashMap<>();
			this.elementMap.put(element.getType(), byType);
		}

		byType.put(element.getId(), element);
	}

	@Override
	public void saveAll(List<T> elements) {
		for (T element : elements) {
			save(element);
		}
	}

	@Override
	public void update(T element) {
		save(element);
	}

	@Override
	public void updateAll(List<T> elements) {
		for (T element : elements) {
			update(element);
		}
	}

	@Override
	public void remove(T element) {
		Map<String, T> byType = this.elementMap.get(element.getType());
		if (byType != null) {
			byType.remove(element.getId());
		}
	}

	@Override
	public void removeAll(List<T> elements) {
		for (T element : elements) {
			remove(element);
		}
	}
}
