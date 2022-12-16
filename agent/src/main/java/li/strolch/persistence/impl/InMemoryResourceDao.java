package li.strolch.persistence.impl;

import java.util.List;
import java.util.Set;

import li.strolch.model.Resource;
import li.strolch.persistence.api.ResourceDao;
import li.strolch.persistence.api.StrolchPersistenceException;

public class InMemoryResourceDao implements ResourceDao {

	private static final InMemoryStrolchDao<Resource> instance = new InMemoryStrolchDao<>();

	@Override
	public boolean supportsPaging() {
		return instance.supportsPaging();
	}

	@Override
	public long querySize() {
		return instance.querySize();
	}

	@Override
	public long querySize(String... types) {
		return instance.querySize(types);
	}

	@Override
	public Set<String> queryTypes() throws StrolchPersistenceException {
		return instance.queryTypes();
	}

	@Override
	public List<Resource> queryAll() throws StrolchPersistenceException {
		return instance.queryAll();
	}

	@Override
	public List<Resource> queryAll(long limit, long offset) throws StrolchPersistenceException {
		return instance.queryAll(limit, offset);
	}

	@Override
	public List<Resource> queryAll(String... types) throws StrolchPersistenceException {
		return instance.queryAll(types);
	}

	@Override
	public List<Resource> queryAll(long limit, long offset, String... types) throws StrolchPersistenceException {
		return instance.queryAll(limit, offset, types);
	}

	@Override
	public void save(Resource element) throws StrolchPersistenceException {
		instance.save(element);
	}

	@Override
	public void saveAll(List<Resource> elements) throws StrolchPersistenceException {
		instance.saveAll(elements);
	}

	@Override
	public void update(Resource element) throws StrolchPersistenceException {
		instance.update(element);
	}

	@Override
	public void updateAll(List<Resource> elements) throws StrolchPersistenceException {
		instance.updateAll(elements);
	}

	@Override
	public void remove(Resource element) throws StrolchPersistenceException {
		instance.remove(element);
	}

	@Override
	public void removeAll(List<Resource> elements) throws StrolchPersistenceException {
		instance.removeAll(elements);
	}

	@Override
	public long removeAll() throws StrolchPersistenceException {
		return instance.removeAll();
	}

	@Override
	public long removeAllBy(String type) throws StrolchPersistenceException {
		return instance.removeAllBy(type);
	}

	@Override
	public Resource queryBy(String type, String id, int version) throws StrolchPersistenceException {
		return instance.queryBy(type, id, version);
	}

	@Override
	public List<Resource> queryVersionsFor(String type, String id) throws StrolchPersistenceException {
		return instance.queryVersionsFor(type, id);
	}

	@Override
	public int queryLatestVersionFor(String type, String id) throws StrolchPersistenceException {
		return instance.queryLatestVersionFor(type, id);
	}

	@Override
	public long queryVersionsSizeFor(String type, String id) throws StrolchPersistenceException {
		return instance.queryVersionsSizeFor(type, id);
	}

	@Override
	public void removeVersion(Resource element) throws StrolchPersistenceException {
		instance.removeVersion(element);
	}

	@Override
	public void flush() throws StrolchPersistenceException {
		instance.flush();
	}
}
