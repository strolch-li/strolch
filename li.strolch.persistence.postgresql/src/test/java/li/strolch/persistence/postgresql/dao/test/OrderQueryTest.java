package li.strolch.persistence.postgresql.dao.test;

import static li.strolch.persistence.postgresql.dao.test.CachedDaoTest.CONFIG_SRC;
import static li.strolch.persistence.postgresql.dao.test.CachedDaoTest.DB_PASSWORD;
import static li.strolch.persistence.postgresql.dao.test.CachedDaoTest.DB_STORE_PATH_DIR;
import static li.strolch.persistence.postgresql.dao.test.CachedDaoTest.DB_URL;
import static li.strolch.persistence.postgresql.dao.test.CachedDaoTest.DB_USERNAME;
import static li.strolch.persistence.postgresql.dao.test.CachedDaoTest.RUNTIME_PATH;
import static li.strolch.persistence.postgresql.dao.test.CachedDaoTest.dropSchema;

import java.io.File;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.List;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;

import li.strolch.agent.api.OrderMap;
import li.strolch.agent.api.StrolchRealm;
import li.strolch.model.ModelGenerator;
import li.strolch.model.Order;
import li.strolch.model.State;
import li.strolch.model.query.DateSelection;
import li.strolch.model.query.IdSelection;
import li.strolch.model.query.NameSelection;
import li.strolch.model.query.OrderQuery;
import li.strolch.model.query.OrderStateSelection;
import li.strolch.model.query.ParameterBagSelection;
import li.strolch.model.query.ParameterBagSelection.NullParameterBagSelection;
import li.strolch.model.query.ParameterSelection;
import li.strolch.model.query.ordering.OrderById;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Certificate;
import li.strolch.runtime.StrolchConstants;
import li.strolch.testbase.runtime.RuntimeMock;
import li.strolch.utils.StringMatchMode;
import li.strolch.utils.collections.DateRange;

public class OrderQueryTest extends QueryTest {

	private static RuntimeMock runtimeMock;

	private static Date past;
	private static Date earlier;
	private static Date current;
	private static Date later;
	private static Date future;

	@BeforeClass
	public static void beforeClass() throws Exception {

		dropSchema(DB_URL, DB_USERNAME, DB_PASSWORD);

		File rootPath = new File(RUNTIME_PATH);
		File configSrc = new File(CONFIG_SRC);
		runtimeMock = new RuntimeMock();
		runtimeMock.mockRuntime(rootPath, configSrc);
		new File(rootPath, DB_STORE_PATH_DIR).mkdir();
		runtimeMock.startContainer();

		Calendar cal = Calendar.getInstance();
		cal.clear();
		cal.set(2000, 1, 1);
		past = cal.getTime();
		cal.set(2000, 4, 1);
		earlier = cal.getTime();
		cal.set(2000, 6, 1);
		current = cal.getTime();
		cal.set(2000, 8, 1);
		later = cal.getTime();
		cal.set(2000, 11, 1);
		future = cal.getTime();

		Certificate cert = runtimeMock.getPrivilegeHandler().authenticate("test", "test".toCharArray());
		StrolchRealm realm = runtimeMock.getRealm(StrolchConstants.DEFAULT_REALM);
		try (StrolchTransaction tx = realm.openTx(cert, "test")) {
			OrderMap orderMap = tx.getOrderMap();

			orderMap.add(tx, ModelGenerator.createOrder("@1", "Order 1", "MyType1", earlier, State.CREATED));
			orderMap.add(tx, ModelGenerator.createOrder("@2", "Order 2", "MyType1", current, State.EXECUTION));
			orderMap.add(tx, ModelGenerator.createOrder("@3", "Order 3", "MyType1", later, State.CLOSED));
			orderMap.add(tx, ModelGenerator.createOrder("@4", "Order 4", "MyType2", earlier, State.CREATED));
			orderMap.add(tx, ModelGenerator.createOrder("@5", "Order 5", "MyType2", current, State.CLOSED));
			orderMap.add(tx, ModelGenerator.createOrder("@6", "Order 6", "MyType2", later, State.CLOSED));

			tx.commitOnClose();
		}
	}

	@AfterClass
	public static void afterClass() {
		if (runtimeMock != null)
			runtimeMock.destroyRuntime();
	}

	public Connection openConn() throws SQLException {
		String url = "jdbc:postgresql://localhost/testdb";
		String username = "testuser";
		String password = "test";
		Connection connection = DriverManager.getConnection(url, username, password);
		connection.setAutoCommit(false);
		return connection;
	}

	@Test
	public void shouldQueryOrderAll() throws SQLException {

		OrderQuery<Order> query = OrderQuery.query("MyType1", new OrderById());
		query.withAny();
		performOrderQuery(query, Arrays.asList("@1", "@2", "@3"));
	}

	@Test
	public void shouldQueryOrderByDate() throws SQLException {

		// range
		OrderQuery<Order> query = OrderQuery.query("MyType1", new OrderById());
		query.and().with(new DateSelection().from(earlier, false).to(later, false));
		performOrderQuery(query, Arrays.asList("@1", "@2", "@3"));

		// equals current
		query = OrderQuery.query("MyType1");
		query.and().with(new DateSelection().from(current, false).to(current, false));
		performOrderQuery(query, Arrays.asList("@2"));

		// equals later
		query = OrderQuery.query("MyType1");
		query.and().with(new DateSelection().from(later, false).to(later, false));
		performOrderQuery(query, Arrays.<String> asList("@3"));

		// equals earlier
		query = OrderQuery.query("MyType1");
		query.and().with(new DateSelection().from(earlier, false).to(earlier, false));
		performOrderQuery(query, Arrays.<String> asList("@1"));

		// past
		query = OrderQuery.query("MyType1");
		query.and().with(new DateSelection().to(past, false));
		performOrderQuery(query, Arrays.<String> asList());

		// future
		query = OrderQuery.query("MyType1");
		query.and().with(new DateSelection().from(future, false));
		performOrderQuery(query, Arrays.<String> asList());

		// earlier
		query = OrderQuery.query("MyType1");
		query.and().with(new DateSelection().from(past, false).to(earlier, true));
		performOrderQuery(query, Arrays.<String> asList("@1"));

		// later
		query = OrderQuery.query("MyType1");
		query.and().with(new DateSelection().from(later, false).to(future, true));
		performOrderQuery(query, Arrays.<String> asList("@3"));
	}

	@Test
	public void shouldQueryOrderByState() throws SQLException {

		OrderQuery<Order> query = OrderQuery.query("MyType1");
		query.and().with(new OrderStateSelection(State.CREATED));
		performOrderQuery(query, Arrays.asList("@1"));

		query = OrderQuery.query("MyType1");
		query.and().with(new OrderStateSelection(State.EXECUTION));
		performOrderQuery(query, Arrays.<String> asList("@2"));
	}

	@Test
	public void shouldQueryOrder1() throws SQLException {

		OrderQuery<Order> query = OrderQuery.query("MyType1", new OrderById());
		query.and().with(new IdSelection("@1", "@2"),
				new NameSelection("Order 1", StringMatchMode.EQUALS_CASE_SENSITIVE));
		performOrderQuery(query, Arrays.asList("@1"));
	}

	@Test
	public void shouldQueryOrder2() throws SQLException {

		OrderQuery<Order> query = OrderQuery.query("MyType1", new OrderById());
		query.or().with(new IdSelection("@1", "@2"),
				new NameSelection("order 1", StringMatchMode.EQUALS_CASE_SENSITIVE));
		performOrderQuery(query, Arrays.asList("@1", "@2"));
	}

	@Test
	public void shouldQueryOrderByBooleParam() throws SQLException {
		OrderQuery<Order> query = OrderQuery.query("MyType1", new OrderById());
		query.and().with(ParameterSelection.booleanSelection("@bag01", "@param1", true));
		performOrderQuery(query, Arrays.asList("@1", "@2", "@3"));
	}

	@Test
	public void shouldQueryOrderByFloatParam() throws SQLException {
		OrderQuery<Order> query = OrderQuery.query("MyType1", new OrderById());
		query.and().with(ParameterSelection.floatSelection("@bag01", "@param2", 44.3));
		performOrderQuery(query, Arrays.asList("@1", "@2", "@3"));
	}

	@Test
	public void shouldQueryOrderByIntegerParam() throws SQLException {
		OrderQuery<Order> query = OrderQuery.query("MyType1", new OrderById());
		query.and().with(ParameterSelection.integerSelection("@bag01", "@param3", 77));
		performOrderQuery(query, Arrays.asList("@1", "@2", "@3"));
	}

	@Test
	public void shouldQueryOrderByLongParam() throws SQLException {
		OrderQuery<Order> query = OrderQuery.query("MyType2", new OrderById());
		query.and().with(ParameterSelection.longSelection("@bag01", "@param4", 4453234566L));
		performOrderQuery(query, Arrays.asList("@4", "@5", "@6"));
	}

	@Test
	public void shouldQueryOrderByStringParam() throws SQLException {

		List<String> expected = Arrays.asList("@1", "@2", "@3");

		OrderQuery<Order> query = OrderQuery.query("MyType1", new OrderById());
		query.and().with(ParameterSelection.stringSelection("@bag01", "@param5", "Strolch",
				StringMatchMode.EQUALS_CASE_SENSITIVE));
		performOrderQuery(query, expected);

		query = OrderQuery.query("MyType1", new OrderById());
		query.and().with(ParameterSelection.stringSelection("@bag01", "@param5", "strolch",
				StringMatchMode.EQUALS_CASE_SENSITIVE));
		performOrderQuery(query, Arrays.<String> asList());

		query = OrderQuery.query("MyType1", new OrderById());
		query.and().with(ParameterSelection.stringSelection("@bag01", "@param5", "strolch",
				StringMatchMode.EQUALS_CASE_INSENSITIVE));
		performOrderQuery(query, expected);

		query = OrderQuery.query("MyType1", new OrderById());
		query.and().with(ParameterSelection.stringSelection("@bag01", "@param5", "olch",
				StringMatchMode.CONTAINS_CASE_INSENSITIVE));
		performOrderQuery(query, expected);
	}

	@Test
	public void shouldQueryOrderByDateParam() throws SQLException {
		OrderQuery<Order> query = OrderQuery.query("MyType1", new OrderById());
		query.and().with(ParameterSelection.dateSelection("@bag01", "@param6", new Date(1354295525628L)));
		performOrderQuery(query, Arrays.asList("@1", "@2", "@3"));
	}

	@Ignore("Not yet implemented")
	@Test
	public void shouldQueryOrderByDateRange() throws SQLException {
		OrderQuery<Order> query = OrderQuery.query("MyType1", new OrderById());
		query.and().with(ParameterSelection.dateRangeSelection("@bag01", "@param6",
				new DateRange().from(new Date(1354295525627L), false).to(new Date(1354295525629L), false)));
		performOrderQuery(query, Arrays.asList("@1", "@2", "@3"));
	}

	@Test
	public void shouldQueryOrderByDurationParam() throws SQLException {
		OrderQuery<Order> query = OrderQuery.query("MyType1", new OrderById());
		query.and().with(ParameterSelection.durationSelection("@bag01", "@param8", "P1D"));
		performOrderQuery(query, Arrays.asList("@1", "@2", "@3"));
	}

	@Test
	public void shouldQueryOrderByNullParam1() throws SQLException {
		OrderQuery<Order> query = OrderQuery.query("MyType1", new OrderById());
		query.and().with(ParameterSelection.nullSelection("@bag01", "@param6"));
		performOrderQuery(query, Arrays.<String> asList());
	}

	@Test
	public void shouldQueryOrderByNullParam2() throws SQLException {
		OrderQuery<Order> query = OrderQuery.query("MyType1", new OrderById());
		query.and().with(ParameterSelection.nullSelection("@bag01", "@param"));
		performOrderQuery(query, Arrays.asList("@1", "@2", "@3"));
	}

	@Test
	public void shouldQueryOrderByBag() throws SQLException {
		OrderQuery<Order> query = OrderQuery.query("MyType1", new OrderById());
		query.and().with(new ParameterBagSelection("@bag01"));
		performOrderQuery(query, Arrays.asList("@1", "@2", "@3"));
	}

	@Test
	public void shouldQueryOrderByNullBag() throws SQLException {
		OrderQuery<Order> query = OrderQuery.query("MyType1", new OrderById());
		query.and().with(new NullParameterBagSelection("@bag01"));
		performOrderQuery(query, Arrays.<String> asList());
	}
}
