package li.strolch.persistence.impl;

import static java.util.stream.Collectors.toList;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import li.strolch.model.StrolchRootElement;
import li.strolch.persistence.api.StrolchDao;
import li.strolch.persistence.api.StrolchPersistenceException;
import li.strolch.utils.collections.MapOfLists;

public class InMemoryStrolchDao<T extends StrolchRootElement> implements StrolchDao<T> {

	private final MapOfLists<String, T> elements;

	public InMemoryStrolchDao() {
		this.elements = new MapOfLists<>();
	}

	public MapOfLists<String, T> getElements() {
		return this.elements;
	}

	@Override
	public boolean supportsPaging() {
		return true;
	}

	@Override
	public long querySize() {
		return this.elements.size();
	}

	@Override
	public long querySize(String... types) {
		long size = 0L;
		for (String type : types) {
			size += this.elements.size(type);
		}
		return size;
	}

	@Override
	public Set<String> queryTypes() throws StrolchPersistenceException {
		return this.elements.keySet();
	}

	@Override
	public List<T> queryAll() throws StrolchPersistenceException {
		return this.elements.values();
	}

	@Override
	public List<T> queryAll(long limit, long offset) throws StrolchPersistenceException {
		return this.elements.values().stream().skip(offset).limit(limit).collect(toList());
	}

	@Override
	public List<T> queryAll(String... types) throws StrolchPersistenceException {
		List<T> values = new ArrayList<>();
		for (String type : types) {
			values.addAll(this.elements.getList(type));
		}
		return values;
	}

	@Override
	public List<T> queryAll(long limit, long offset, String... types) throws StrolchPersistenceException {
		return this.queryAll(types).stream().skip(offset).limit(limit).collect(toList());
	}

	@Override
	public void save(T element) throws StrolchPersistenceException {
		this.elements.addElement(element.getType(), element);
	}

	@Override
	public void saveAll(List<T> elements) throws StrolchPersistenceException {
		elements.forEach(this::save);
	}

	@Override
	public void update(T element) throws StrolchPersistenceException {
		save(element);
	}

	@Override
	public void updateAll(List<T> elements) throws StrolchPersistenceException {
		saveAll(elements);
	}

	@Override
	public void remove(T element) throws StrolchPersistenceException {
		this.elements.removeElement(element.getType(), element);
	}

	@Override
	public void removeAll(List<T> elements) throws StrolchPersistenceException {
		elements.forEach(this::remove);
	}

	@Override
	public long removeAll() throws StrolchPersistenceException {
		int size = this.elements.size();
		this.elements.clear();
		return size;
	}

	@Override
	public long removeAllBy(String type) throws StrolchPersistenceException {
		return this.elements.removeList(type).size();
	}

	@Override
	public T queryBy(String type, String id, int version) throws StrolchPersistenceException {
		throw new UnsupportedOperationException("Versioning is not supported!");
	}

	@Override
	public List<T> queryVersionsFor(String type, String id) throws StrolchPersistenceException {
		throw new UnsupportedOperationException("Versioning is not supported!");
	}

	@Override
	public int queryLatestVersionFor(String type, String id) throws StrolchPersistenceException {
		throw new UnsupportedOperationException("Versioning is not supported!");
	}

	@Override
	public long queryVersionsSizeFor(String type, String id) throws StrolchPersistenceException {
		throw new UnsupportedOperationException("Versioning is not supported!");
	}

	@Override
	public void removeVersion(T element) throws StrolchPersistenceException {
		throw new UnsupportedOperationException("Versioning is not supported!");
	}

	@Override
	public void flush() throws StrolchPersistenceException {
		// do nothing
	}
}
