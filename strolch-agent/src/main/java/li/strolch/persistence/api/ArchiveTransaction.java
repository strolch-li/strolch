package li.strolch.persistence.api;

import li.strolch.exception.StrolchException;
import li.strolch.exception.StrolchModelException;
import li.strolch.model.Order;
import li.strolch.model.Resource;
import li.strolch.model.activity.Activity;
import li.strolch.utils.collections.DateRange;

import java.time.LocalDateTime;
import java.util.Date;
import java.util.List;

import static li.strolch.model.Version.setInitialVersionFor;
import static li.strolch.model.Version.updateVersionFor;

public abstract class ArchiveTransaction implements AutoCloseable {

	private final StrolchTransaction tx;
	private final DataArchiveHandler archiveHandler;
	private final TransactionResult txResult;

	private OrderDao orderDao;
	private ResourceDao resourceDao;
	private ActivityDao activityDao;

	protected ArchiveTransaction(StrolchTransaction tx, DataArchiveHandler archiveHandler) {
		this.tx = tx;
		this.archiveHandler = archiveHandler;
		this.txResult = new TransactionResult(tx.getRealmName(), System.nanoTime(), LocalDateTime.now());
	}

	public StrolchTransaction tx() {
		return this.tx;
	}

	public TransactionResult getTxResult() {
		return this.txResult;
	}

	public long getResourceCount(String... types) {
		ResourceDao dao = getResourceDao();
		return dao.querySize(types);
	}

	public Resource getResourceBy(String type, String id) {
		ResourceDao dao = getResourceDao();
		return dao.queryBy(type, id, dao.queryLatestVersionFor(type, id));
	}

	public long getOrderCount(String... types) {
		OrderDao dao = getOrderDao();
		return dao.querySize(types);
	}

	public long getOrderCount(DateRange dateRange, String... types) {
		OrderDao dao = getOrderDao();
		return dao.querySize(dateRange, types);
	}

	public Order getOrderBy(String type, String id) {
		OrderDao dao = getOrderDao();
		return dao.queryBy(type, id, dao.queryLatestVersionFor(type, id));
	}

	public List<Order> getOrdersBy(String... types) {
		OrderDao dao = getOrderDao();
		return dao.queryAll(types);
	}

	public List<Order> getOrdersBy(long limit, long offset, String... types) {
		OrderDao dao = getOrderDao();
		return dao.queryAll(limit, offset, types);
	}

	public List<Order> getOrdersBy(DateRange dateRange, String... types) {
		OrderDao dao = getOrderDao();
		return dao.queryAll(dateRange, types);
	}

	public List<Order> getOrdersBy(DateRange dateRange, long limit, long offset, boolean asc, String... types) {
		OrderDao dao = getOrderDao();
		return dao.queryAll(dateRange, limit, offset, asc, types);
	}

	public long getActivityCount(String... types) {
		ActivityDao dao = getActivityDao();
		return dao.querySize(types);
	}

	public Activity getActivityBy(String type, String id) {
		ActivityDao dao = getActivityDao();
		return dao.queryBy(type, id, dao.queryLatestVersionFor(type, id));
	}

	public void addResource(Resource resource) throws StrolchModelException {
		if (!resource.hasVersion())
			setInitialVersionFor(resource, this.tx.getUsername());
		getResourceDao().save(resource);
	}

	public void addResources(List<Resource> resources) throws StrolchModelException {
		resources.forEach(resource -> {
			if (!resource.hasVersion())
				setInitialVersionFor(resource, this.tx.getUsername());
		});
		getResourceDao().saveAll(resources);
	}

	public void addOrder(Order order) throws StrolchException {
		if (!order.hasVersion())
			setInitialVersionFor(order, this.tx.getUsername());
		getOrderDao().save(order);
	}

	public void addOrders(List<Order> orders) throws StrolchModelException {
		orders.forEach(order -> {
			if (!order.hasVersion())
				setInitialVersionFor(order, this.tx.getUsername());
		});
		getOrderDao().saveAll(orders);
	}

	public void addActivity(Activity activity) throws StrolchException {
		if (!activity.hasVersion())
			setInitialVersionFor(activity, this.tx.getUsername());
		getActivityDao().save(activity);
	}

	public void addActivities(List<Activity> activities) throws StrolchModelException {
		activities.forEach(activity -> {
			if (!activity.hasVersion())
				setInitialVersionFor(activity, this.tx.getUsername());
		});
		getActivityDao().saveAll(activities);
	}

	public void updateResource(Resource resource) throws StrolchException {
		if (!resource.hasVersion()) {
			int version = getResourceDao().queryLatestVersionFor(resource.getType(), resource.getId());
			updateVersionFor(resource, version, tx.getUsername(), false);
		}
		getResourceDao().update(resource);
	}

	public void updateOrder(Order order) {
		if (!order.hasVersion()) {
			int version = getOrderDao().queryLatestVersionFor(order.getType(), order.getId());
			updateVersionFor(order, version, tx.getUsername(), false);
		}
		getOrderDao().update(order);
	}

	public void updateActivity(Activity activity) throws StrolchException {
		if (!activity.hasVersion()) {
			int version = getActivityDao().queryLatestVersionFor(activity.getType(), activity.getId());
			updateVersionFor(activity, version, tx.getUsername(), false);
		}
		getActivityDao().update(activity);
	}

	public void removeResource(Resource resource) throws StrolchException {
		if (!resource.hasVersion()) {
			int version = getResourceDao().queryLatestVersionFor(resource.getType(), resource.getId());
			updateVersionFor(resource, version, tx.getUsername(), false);
		}
		getResourceDao().remove(resource);
	}

	public void removeOrder(Order order) throws StrolchException {
		if (!order.hasVersion()) {
			int version = getOrderDao().queryLatestVersionFor(order.getType(), order.getId());
			updateVersionFor(order, version, tx.getUsername(), false);
		}
		getOrderDao().remove(order);
	}

	public void removeActivity(Activity activity) throws StrolchException {
		if (!activity.hasVersion()) {
			int version = getActivityDao().queryLatestVersionFor(activity.getType(), activity.getId());
			updateVersionFor(activity, version, tx.getUsername(), false);
		}
		getActivityDao().remove(activity);
	}

	public synchronized OrderDao getOrderDao() {
		if (this.orderDao == null)
			this.orderDao = this.archiveHandler.getOrderDao(this);
		return this.orderDao;
	}

	public synchronized ResourceDao getResourceDao() {
		if (this.resourceDao == null)
			this.resourceDao = this.archiveHandler.getResourceDao(this);
		return this.resourceDao;
	}

	public synchronized ActivityDao getActivityDao() {
		if (this.activityDao == null)
			this.activityDao = this.archiveHandler.getActivityDao(this);
		return this.activityDao;
	}

	public void flush() {
		if (this.orderDao != null)
			this.orderDao.flush();
		if (this.resourceDao != null)
			this.resourceDao.flush();
		if (this.activityDao != null)
			this.activityDao.flush();
	}
}
