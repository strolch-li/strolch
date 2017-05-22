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
import java.util.Date;
import java.util.List;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;

import li.strolch.agent.api.ActivityMap;
import li.strolch.agent.api.StrolchRealm;
import li.strolch.model.ModelGenerator;
import li.strolch.model.State;
import li.strolch.model.activity.Action;
import li.strolch.model.activity.Activity;
import li.strolch.model.activity.TimeOrdering;
import li.strolch.model.query.ActivityQuery;
import li.strolch.model.query.ActivityStateSelection;
import li.strolch.model.query.IdSelection;
import li.strolch.model.query.NameSelection;
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

public class ActivityQueryTest extends QueryTest {

	private static RuntimeMock runtimeMock;

	@BeforeClass
	public static void beforeClass() throws Exception {

		dropSchema(DB_URL, DB_USERNAME, DB_PASSWORD);

		File rootPath = new File(RUNTIME_PATH);
		File configSrc = new File(CONFIG_SRC);
		runtimeMock = new RuntimeMock();
		runtimeMock.mockRuntime(rootPath, configSrc);
		new File(rootPath, DB_STORE_PATH_DIR).mkdir();
		runtimeMock.startContainer();

		Certificate cert = runtimeMock.getPrivilegeHandler().authenticate("test", "test".toCharArray());
		StrolchRealm realm = runtimeMock.getRealm(StrolchConstants.DEFAULT_REALM);
		try (StrolchTransaction tx = realm.openTx(cert, "test")) {
			ActivityMap activityMap = tx.getActivityMap();

			Activity activity = ModelGenerator.createActivity("@1", "Activity 1", "MyType1", TimeOrdering.SERIES);
			activityMap.add(tx, activity);

			activity = ModelGenerator.createActivity("@2", "Activity 2", "MyType1", TimeOrdering.SERIES);
			((Action) activity.getElement("action_" + activity.getId())).setState(State.EXECUTION);
			activityMap.add(tx, activity);

			activity = ModelGenerator.createActivity("@3", "Activity 3", "MyType1", TimeOrdering.SERIES);
			((Action) activity.getElement("action_" + activity.getId())).setState(State.CLOSED);
			activityMap.add(tx, activity);

			activity = ModelGenerator.createActivity("@4", "Activity 4", "MyType2", TimeOrdering.SERIES);
			((Action) activity.getElement("action_" + activity.getId())).setState(State.CREATED);
			activityMap.add(tx, activity);

			activity = ModelGenerator.createActivity("@5", "Activity 5", "MyType2", TimeOrdering.SERIES);
			((Action) activity.getElement("action_" + activity.getId())).setState(State.CLOSED);
			activityMap.add(tx, activity);

			activity = ModelGenerator.createActivity("@6", "Activity 6", "MyType2", TimeOrdering.SERIES);
			((Action) activity.getElement("action_" + activity.getId())).setState(State.CLOSED);
			activityMap.add(tx, activity);

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
	public void shouldQueryActivityAll() throws SQLException {

		ActivityQuery<Activity> query = ActivityQuery.query("MyType1", new OrderById());
		query.withAny();
		performActivityQuery(query, Arrays.asList("@1", "@2", "@3"));
	}

	@Test
	public void shouldQueryActivityByState() throws SQLException {

		ActivityQuery<Activity> query = ActivityQuery.query("MyType1");
		query.and().with(new ActivityStateSelection(State.CREATED));
		performActivityQuery(query, Arrays.asList("@1"));

		query = ActivityQuery.query("MyType1");
		query.and().with(new ActivityStateSelection(State.EXECUTION));
		performActivityQuery(query, Arrays.<String> asList("@2"));
	}

	@Test
	public void shouldQueryActivity1() throws SQLException {

		ActivityQuery<Activity> query = ActivityQuery.query("MyType1", new OrderById());
		query.and().with(new IdSelection("@1", "@2"),
				new NameSelection("Activity 1", StringMatchMode.EQUALS_CASE_SENSITIVE));
		performActivityQuery(query, Arrays.asList("@1"));
	}

	@Test
	public void shouldQueryActivity2() throws SQLException {

		ActivityQuery<Activity> query = ActivityQuery.query("MyType1", new OrderById());
		query.or().with(new IdSelection("@1", "@2"),
				new NameSelection("activity 1", StringMatchMode.EQUALS_CASE_SENSITIVE));
		performActivityQuery(query, Arrays.asList("@1", "@2"));
	}

	@Test
	public void shouldQueryActivityByBooleParam() throws SQLException {
		ActivityQuery<Activity> query = ActivityQuery.query("MyType1", new OrderById());
		query.and().with(ParameterSelection.booleanSelection("@bag01", "@param1", true));
		performActivityQuery(query, Arrays.asList("@1", "@2", "@3"));
	}

	@Test
	public void shouldQueryActivityByFloatParam() throws SQLException {
		ActivityQuery<Activity> query = ActivityQuery.query("MyType1", new OrderById());
		query.and().with(ParameterSelection.floatSelection("@bag01", "@param2", 44.3));
		performActivityQuery(query, Arrays.asList("@1", "@2", "@3"));
	}

	@Test
	public void shouldQueryActivityByIntegerParam() throws SQLException {
		ActivityQuery<Activity> query = ActivityQuery.query("MyType1", new OrderById());
		query.and().with(ParameterSelection.integerSelection("@bag01", "@param3", 77));
		performActivityQuery(query, Arrays.asList("@1", "@2", "@3"));
	}

	@Test
	public void shouldQueryActivityByLongParam() throws SQLException {
		ActivityQuery<Activity> query = ActivityQuery.query("MyType2", new OrderById());
		query.and().with(ParameterSelection.longSelection("@bag01", "@param4", 4453234566L));
		performActivityQuery(query, Arrays.asList("@4", "@5", "@6"));
	}

	@Test
	public void shouldQueryActivityByStringParam() throws SQLException {

		List<String> expected = Arrays.asList("@1", "@2", "@3");

		ActivityQuery<Activity> query = ActivityQuery.query("MyType1", new OrderById());
		query.and().with(ParameterSelection.stringSelection("@bag01", "@param5", "Strolch",
				StringMatchMode.EQUALS_CASE_SENSITIVE));
		performActivityQuery(query, expected);

		query = ActivityQuery.query("MyType1", new OrderById());
		query.and().with(ParameterSelection.stringSelection("@bag01", "@param5", "strolch",
				StringMatchMode.EQUALS_CASE_SENSITIVE));
		performActivityQuery(query, Arrays.<String> asList());

		query = ActivityQuery.query("MyType1", new OrderById());
		query.and().with(ParameterSelection.stringSelection("@bag01", "@param5", "strolch",
				StringMatchMode.EQUALS_CASE_INSENSITIVE));
		performActivityQuery(query, expected);

		query = ActivityQuery.query("MyType1", new OrderById());
		query.and().with(ParameterSelection.stringSelection("@bag01", "@param5", "olch",
				StringMatchMode.CONTAINS_CASE_INSENSITIVE));
		performActivityQuery(query, expected);
	}

	@Test
	public void shouldQueryActivityByDateParam() throws SQLException {
		ActivityQuery<Activity> query = ActivityQuery.query("MyType1", new OrderById());
		query.and().with(ParameterSelection.dateSelection("@bag01", "@param6", new Date(1354295525628L)));
		performActivityQuery(query, Arrays.asList("@1", "@2", "@3"));
	}

	@Ignore("Not yet implemented")
	@Test
	public void shouldQueryActivityByDateRange() throws SQLException {
		ActivityQuery<Activity> query = ActivityQuery.query("MyType1", new OrderById());
		query.and().with(ParameterSelection.dateRangeSelection("@bag01", "@param6",
				new DateRange().from(new Date(1354295525627L), false).to(new Date(1354295525629L), false)));
		performActivityQuery(query, Arrays.asList("@1", "@2", "@3"));
	}

	@Test
	public void shouldQueryActivityByDurationParam() throws SQLException {
		ActivityQuery<Activity> query = ActivityQuery.query("MyType1", new OrderById());
		query.and().with(ParameterSelection.durationSelection("@bag01", "@param8", "P1D"));
		performActivityQuery(query, Arrays.asList("@1", "@2", "@3"));
	}

	@Test
	public void shouldQueryActivityByNullParam1() throws SQLException {
		ActivityQuery<Activity> query = ActivityQuery.query("MyType1", new OrderById());
		query.and().with(ParameterSelection.nullSelection("@bag01", "@param6"));
		performActivityQuery(query, Arrays.<String> asList());
	}

	@Test
	public void shouldQueryActivityByNullParam2() throws SQLException {
		ActivityQuery<Activity> query = ActivityQuery.query("MyType1", new OrderById());
		query.and().with(ParameterSelection.nullSelection("@bag01", "@param"));
		performActivityQuery(query, Arrays.asList("@1", "@2", "@3"));
	}

	@Test
	public void shouldQueryActivityByBag() throws SQLException {
		ActivityQuery<Activity> query = ActivityQuery.query("MyType1", new OrderById());
		query.and().with(new ParameterBagSelection("@bag01"));
		performActivityQuery(query, Arrays.asList("@1", "@2", "@3"));
	}

	@Test
	public void shouldQueryActivityByNullBag() throws SQLException {
		ActivityQuery<Activity> query = ActivityQuery.query("MyType1", new OrderById());
		query.and().with(new NullParameterBagSelection("@bag01"));
		performActivityQuery(query, Arrays.<String> asList());
	}
}
