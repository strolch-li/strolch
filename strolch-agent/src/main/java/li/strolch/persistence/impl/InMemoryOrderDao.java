package li.strolch.persistence.impl;

import static java.util.stream.Collectors.toList;

import java.util.List;
import java.util.Set;

import li.strolch.model.Order;
import li.strolch.persistence.api.OrderDao;
import li.strolch.persistence.api.StrolchPersistenceException;
import li.strolch.utils.collections.DateRange;

public class InMemoryOrderDao implements OrderDao {

	private static final InMemoryStrolchDao<Order> instance = new InMemoryStrolchDao<>();

	@Override
	public boolean supportsPaging() {
		return instance.supportsPaging();
	}

	@Override
	public long querySize() {
		return instance.querySize();
	}

	@Override
	public long querySize(DateRange dateRange) {
		return instance.getElements().values().stream().filter(o -> dateRange.contains(o.getDate())).count();
	}

	@Override
	public long querySize(String... types) {
		return instance.querySize(types);
	}

	@Override
	public long querySize(DateRange dateRange, String... types) {
		return instance.getElements().values().stream().filter(o -> isIn(o, types))
				.filter(o -> dateRange.contains(o.getDate())).count();
	}

	private boolean isIn(Order o, String[] types) {
		for (String type : types) {
			if (o.getType().equals(type))
				return true;
		}
		return false;
	}

	@Override
	public Set<String> queryTypes() throws StrolchPersistenceException {
		return instance.queryTypes();
	}

	@Override
	public List<Order> queryAll() throws StrolchPersistenceException {
		return instance.queryAll();
	}

	@Override
	public List<Order> queryAll(DateRange dateRange) throws StrolchPersistenceException {
		return instance.getElements().values().stream().filter(o -> dateRange.contains(o.getDate())).collect(toList());
	}

	@Override
	public List<Order> queryAll(long limit, long offset) throws StrolchPersistenceException {
		return instance.queryAll(limit, offset);
	}

	@Override
	public List<Order> queryAll(DateRange dateRange, long limit, long offset, boolean asc)
			throws StrolchPersistenceException {
		return instance.getElements().values().stream().filter(o -> dateRange.contains(o.getDate())).skip(offset)
				.limit(limit).collect(toList());
	}

	@Override
	public List<Order> queryAll(String... types) throws StrolchPersistenceException {
		return instance.queryAll(types);
	}

	@Override
	public List<Order> queryAll(long limit, long offset, String... types) throws StrolchPersistenceException {
		return instance.queryAll(limit, offset, types);
	}

	@Override
	public List<Order> queryAll(DateRange dateRange, String... types) throws StrolchPersistenceException {
		return instance.getElements().values().stream().filter(o -> isIn(o, types))
				.filter(o -> dateRange.contains(o.getDate())).collect(toList());
	}

	@Override
	public List<Order> queryAll(DateRange dateRange, long limit, long offset, boolean asc, String... types)
			throws StrolchPersistenceException {
		return instance.getElements().values().stream().filter(o -> isIn(o, types))
				.filter(o -> dateRange.contains(o.getDate())).skip(offset).limit(limit).collect(toList());
	}

	@Override
	public void save(Order element) throws StrolchPersistenceException {
		instance.save(element);
	}

	@Override
	public void saveAll(List<Order> elements) throws StrolchPersistenceException {
		instance.saveAll(elements);
	}

	@Override
	public void update(Order element) throws StrolchPersistenceException {
		instance.update(element);
	}

	@Override
	public void updateAll(List<Order> elements) throws StrolchPersistenceException {
		instance.updateAll(elements);
	}

	@Override
	public void remove(Order element) throws StrolchPersistenceException {
		instance.remove(element);
	}

	@Override
	public void removeAll(List<Order> elements) throws StrolchPersistenceException {
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
	public Order queryBy(String type, String id, int version) throws StrolchPersistenceException {
		return instance.queryBy(type, id, version);
	}

	@Override
	public List<Order> queryVersionsFor(String type, String id) throws StrolchPersistenceException {
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
	public void removeVersion(Order element) throws StrolchPersistenceException {
		instance.removeVersion(element);
	}

	@Override
	public void flush() throws StrolchPersistenceException {
		instance.flush();
	}
}
