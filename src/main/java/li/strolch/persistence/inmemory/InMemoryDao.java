package li.strolch.persistence.inmemory;

import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import li.strolch.model.StrolchElement;
import li.strolch.persistence.api.StrolchDao;
import li.strolch.persistence.api.StrolchPersistenceException;

public class InMemoryDao<T extends StrolchElement> implements StrolchDao<T> {

	private Map<String, Map<String, T>> elementMap;

	public InMemoryDao() {
		this.elementMap = new HashMap<>();
	}

	@Override
	public long querySize() {
		long size = 0;
		for (String type : this.elementMap.keySet()) {
			Map<String, T> byType = this.elementMap.get(type);
			size += byType.size();
		}
		return size;
	}

	@Override
	public long querySize(String type) {
		Map<String, T> byType = this.elementMap.get(type);
		if (byType == null)
			return 0;
		return byType.size();
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
		Map<String, T> byType = this.elementMap.get(type);
		if (byType == null)
			return Collections.emptySet();
		return byType.keySet();
	}

	@Override
	public Set<String> queryTypes() {
		return this.elementMap.keySet();
	}

	@Override
	public T queryBy(String type, String id) {
		Map<String, T> byType = this.elementMap.get(type);
		if (byType == null)
			return null;
		return byType.get(id);
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
		Map<String, T> byType = this.elementMap.get(type);
		if (byType == null)
			return Collections.emptyList();
		return new ArrayList<>(byType.values());
	}

	@Override
	public void save(T element) {
		Map<String, T> byType = this.elementMap.get(element.getType());
		if (byType == null) {
			byType = new HashMap<>();
			this.elementMap.put(element.getType(), byType);
		}

		if (byType.containsKey(element.getId())) {
			String msg = "An element already exists with the id {0}. Elements of the same class must always have a unique id, regardless of their type!"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, element.getId());
			throw new StrolchPersistenceException(msg);
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
		Map<String, T> byType = this.elementMap.get(element.getType());
		if (byType == null) {
			byType = new HashMap<>();
			this.elementMap.put(element.getType(), byType);
		}

		byType.put(element.getId(), element);
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

			if (byType.isEmpty()) {
				this.elementMap.remove(element.getType());
			}
		}
	}

	@Override
	public void removeAll(List<T> elements) {
		for (T element : elements) {
			remove(element);
		}
	}

	@Override
	public long removeAll() {
		long removed = 0;

		Set<String> keySet = new HashSet<String>(this.elementMap.keySet());
		for (String type : keySet) {
			Map<String, T> byType = this.elementMap.remove(type);
			removed += byType.size();
			byType.clear();
		}

		return removed;
	}

	@Override
	public long removeAllBy(String type) {
		Map<String, T> byType = this.elementMap.remove(type);
		if (byType == null)
			return 0;
		long removed = byType.size();
		byType.clear();
		return removed;
	}
}
