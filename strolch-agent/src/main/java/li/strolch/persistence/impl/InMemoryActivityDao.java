package li.strolch.persistence.impl;

import java.util.List;
import java.util.Set;

import li.strolch.model.activity.Activity;
import li.strolch.persistence.api.ActivityDao;
import li.strolch.persistence.api.StrolchPersistenceException;

public class InMemoryActivityDao implements ActivityDao {

	private static final InMemoryStrolchDao<Activity> instance = new InMemoryStrolchDao<>();

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
	public List<Activity> queryAll() throws StrolchPersistenceException {
		return instance.queryAll();
	}

	@Override
	public List<Activity> queryAll(long limit, long offset) throws StrolchPersistenceException {
		return instance.queryAll(limit, offset);
	}

	@Override
	public List<Activity> queryAll(String... types) throws StrolchPersistenceException {
		return instance.queryAll(types);
	}

	@Override
	public List<Activity> queryAll(long limit, long offset, String... types) throws StrolchPersistenceException {
		return instance.queryAll(limit, offset, types);
	}

	@Override
	public void save(Activity element) throws StrolchPersistenceException {
		instance.save(element);
	}

	@Override
	public void saveAll(List<Activity> elements) throws StrolchPersistenceException {
		instance.saveAll(elements);
	}

	@Override
	public void update(Activity element) throws StrolchPersistenceException {
		instance.update(element);
	}

	@Override
	public void updateAll(List<Activity> elements) throws StrolchPersistenceException {
		instance.updateAll(elements);
	}

	@Override
	public void remove(Activity element) throws StrolchPersistenceException {
		instance.remove(element);
	}

	@Override
	public void removeAll(List<Activity> elements) throws StrolchPersistenceException {
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
	public Activity queryBy(String type, String id, int version) throws StrolchPersistenceException {
		return instance.queryBy(type, id, version);
	}

	@Override
	public List<Activity> queryVersionsFor(String type, String id) throws StrolchPersistenceException {
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
	public void removeVersion(Activity element) throws StrolchPersistenceException {
		instance.removeVersion(element);
	}

	@Override
	public void flush() throws StrolchPersistenceException {
		instance.flush();
	}
}
