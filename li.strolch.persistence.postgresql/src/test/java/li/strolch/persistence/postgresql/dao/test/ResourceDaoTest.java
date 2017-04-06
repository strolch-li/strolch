package li.strolch.persistence.postgresql.dao.test;

import static li.strolch.persistence.postgresql.dao.test.CachedDaoTest.CONFIG_SRC;
import static li.strolch.persistence.postgresql.dao.test.CachedDaoTest.DB_PASSWORD;
import static li.strolch.persistence.postgresql.dao.test.CachedDaoTest.DB_STORE_PATH_DIR;
import static li.strolch.persistence.postgresql.dao.test.CachedDaoTest.DB_URL;
import static li.strolch.persistence.postgresql.dao.test.CachedDaoTest.DB_USERNAME;
import static li.strolch.persistence.postgresql.dao.test.CachedDaoTest.RUNTIME_PATH;
import static li.strolch.persistence.postgresql.dao.test.CachedDaoTest.dropSchema;

import java.io.File;
import java.sql.SQLException;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;

import li.strolch.agent.api.ResourceMap;
import li.strolch.agent.api.StrolchRealm;
import li.strolch.model.ModelGenerator;
import li.strolch.model.Resource;
import li.strolch.model.query.IdSelection;
import li.strolch.model.query.NameSelection;
import li.strolch.model.query.OrSelection;
import li.strolch.model.query.ParameterBagSelection;
import li.strolch.model.query.ParameterBagSelection.NullParameterBagSelection;
import li.strolch.model.query.ParameterSelection;
import li.strolch.model.query.ResourceQuery;
import li.strolch.model.query.ordering.OrderById;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Certificate;
import li.strolch.runtime.StrolchConstants;
import li.strolch.testbase.runtime.RuntimeMock;
import li.strolch.utils.StringMatchMode;
import li.strolch.utils.collections.DateRange;
import li.strolch.utils.iso8601.ISO8601FormatFactory;

public class ResourceDaoTest extends QueryTest {

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

		Certificate cert = runtimeMock.getPrivilegeHandler().authenticate("test", "test".getBytes());
		StrolchRealm realm = runtimeMock.getRealm(StrolchConstants.DEFAULT_REALM);
		try (StrolchTransaction tx = realm.openTx(cert, "test")) {

			ResourceMap resourceMap = tx.getResourceMap();
			resourceMap.add(tx, ModelGenerator.createResource("@1", "Resource 1", "MyType1"));
			resourceMap.add(tx, ModelGenerator.createResource("@2", "Resource 2", "MyType1"));
			resourceMap.add(tx, ModelGenerator.createResource("@3", "Resource 3", "MyType1"));
			resourceMap.add(tx, ModelGenerator.createResource("@4", "Resource 4", "MyType2"));
			resourceMap.add(tx, ModelGenerator.createResource("@5", "Resource 5", "MyType2"));
			resourceMap.add(tx, ModelGenerator.createResource("@6", "Resource 6", "MyType2"));

			tx.commitOnClose();
		}
	}

	@AfterClass
	public static void afterClass() {
		if (runtimeMock != null)
			runtimeMock.destroyRuntime();
	}

	@Test
	public void shouldQueryResourceAll() throws SQLException {

		ResourceQuery<Resource> query = ResourceQuery.query("MyType2", new OrderById(false));
		query.withAny();
		performResourceQuery(query, Arrays.asList("@6", "@5", "@4"));
	}

	@Test
	public void shouldQueryResource1() throws SQLException {

		ResourceQuery<Resource> query = ResourceQuery.query("MyType1", new OrderById());
		query.or().with(new IdSelection("@1", "@2"),
				new NameSelection("Resource 1", StringMatchMode.EQUALS_CASE_SENSITIVE));
		performResourceQuery(query, Arrays.asList("@1", "@2"));
	}

	@Test
	public void shouldQueryResource2() throws SQLException {
		ResourceQuery<Resource> query = ResourceQuery.query("MyType1", new OrderById());
		query.and().with(new OrSelection(new IdSelection("@1"), new IdSelection("@2")),
				new OrSelection(new NameSelection("Resource 1", StringMatchMode.EQUALS_CASE_SENSITIVE),
						new NameSelection("Resource 2", StringMatchMode.EQUALS_CASE_SENSITIVE)));
		performResourceQuery(query, Arrays.asList("@1", "@2"));
	}

	@Test
	public void shouldQueryResourceByBooleParam() throws SQLException {
		ResourceQuery<Resource> query = ResourceQuery.query("MyType1", new OrderById());
		query.and().with(ParameterSelection.booleanSelection("@bag01", "@param1", true));
		performResourceQuery(query, Arrays.asList("@1", "@2", "@3"));
	}

	@Test
	public void shouldQueryResourceByFloatParam() throws SQLException {
		ResourceQuery<Resource> query = ResourceQuery.query("MyType1", new OrderById());
		query.and().with(ParameterSelection.floatSelection("@bag01", "@param2", 44.3));
		performResourceQuery(query, Arrays.asList("@1", "@2", "@3"));
	}

	@Test
	public void shouldQueryResourceByIntegerParam() throws SQLException {
		ResourceQuery<Resource> query = ResourceQuery.query("MyType1", new OrderById());
		query.and().with(ParameterSelection.integerSelection("@bag01", "@param3", 77));
		performResourceQuery(query, Arrays.asList("@1", "@2", "@3"));
	}

	@Test
	public void shouldQueryResourceByLongParam() throws SQLException {
		ResourceQuery<Resource> query = ResourceQuery.query("MyType2", new OrderById());
		query.and().with(ParameterSelection.longSelection("@bag01", "@param4", 4453234566L));
		performResourceQuery(query, Arrays.asList("@4", "@5", "@6"));
	}

	@Test
	public void shouldQueryResourceByStringParam() throws SQLException {

		List<String> expected = Arrays.asList("@1", "@2", "@3");

		ResourceQuery<Resource> query = ResourceQuery.query("MyType1", new OrderById());
		query.and().with(ParameterSelection.stringSelection("@bag01", "@param5", "Strolch",
				StringMatchMode.EQUALS_CASE_SENSITIVE));
		performResourceQuery(query, expected);

		query = ResourceQuery.query("MyType1", new OrderById());
		query.and().with(ParameterSelection.stringSelection("@bag01", "@param5", "strolch",
				StringMatchMode.EQUALS_CASE_SENSITIVE));
		performResourceQuery(query, Arrays.<String> asList());

		query = ResourceQuery.query("MyType1", new OrderById());
		query.and().with(ParameterSelection.stringSelection("@bag01", "@param5", "strolch",
				StringMatchMode.EQUALS_CASE_INSENSITIVE));
		performResourceQuery(query, expected);

		query = ResourceQuery.query("MyType1", new OrderById());
		query.and().with(ParameterSelection.stringSelection("@bag01", "@param5", "olch",
				StringMatchMode.CONTAINS_CASE_INSENSITIVE));
		performResourceQuery(query, expected);

		query = ResourceQuery.query("MyType1", new OrderById());
		query.and()
				.with(ParameterSelection.stringSelection("@bag01", "@param5", "olch",
						StringMatchMode.CONTAINS_CASE_INSENSITIVE),
						ParameterSelection.stringSelection("@bag01", "@param5", "strolch",
								StringMatchMode.CONTAINS_CASE_INSENSITIVE),
						ParameterSelection.stringSelection("@bag01", "@param5", "Strolch",
								StringMatchMode.EQUALS_CASE_SENSITIVE));
		performResourceQuery(query, expected);
	}

	@Test
	public void shouldQueryResourceByAnyTypeParam() throws SQLException {

		ResourceQuery<Resource> query = ResourceQuery.query("MyType1", new OrderById());
		query.and().with(ParameterSelection.anyTypeSelection("@bag01", "@param6",
				ISO8601FormatFactory.getInstance().formatDate(new Date(1354295525628L)), StringMatchMode.ci()));
		performResourceQuery(query, Arrays.asList("@1", "@2", "@3"));

		query = ResourceQuery.query("MyType1", new OrderById());
		query.and().with(ParameterSelection.anyTypeSelection("@bag01", "@param8", "P1D", StringMatchMode.ci()));
		performResourceQuery(query, Arrays.asList("@1", "@2", "@3"));
	}

	@Test
	public void shouldQueryResourceByDateParam() throws SQLException {
		ResourceQuery<Resource> query = ResourceQuery.query("MyType1", new OrderById());
		query.and().with(ParameterSelection.dateSelection("@bag01", "@param6", new Date(1354295525628L)));
		performResourceQuery(query, Arrays.asList("@1", "@2", "@3"));
	}

	@Ignore("Not yet implemented")
	@Test
	public void shouldQueryResourceByDateRange() throws SQLException {
		ResourceQuery<Resource> query = ResourceQuery.query("MyType1", new OrderById());
		query.and().with(ParameterSelection.dateRangeSelection("@bag01", "@param6",
				new DateRange().from(new Date(1354295525627L), false).to(new Date(1354295525629L), false)));
		performResourceQuery(query, Arrays.asList("@1", "@2", "@3"));
	}

	@Test
	public void shouldQueryResourceByDurationParam() throws SQLException {
		ResourceQuery<Resource> query = ResourceQuery.query("MyType1", new OrderById());
		query.and().with(ParameterSelection.durationSelection("@bag01", "@param8", "P1D"));
		performResourceQuery(query, Arrays.asList("@1", "@2", "@3"));
	}

	@Test
	public void shouldQueryResourceByNullParam1() throws SQLException {
		ResourceQuery<Resource> query = ResourceQuery.query("MyType1", new OrderById());
		query.and().with(ParameterSelection.nullSelection("@bag01", "@param6"));
		performResourceQuery(query, Arrays.<String> asList());
	}

	@Test
	public void shouldQueryResourceByNullParam2() throws SQLException {
		ResourceQuery<Resource> query = ResourceQuery.query("MyType1", new OrderById());
		query.and().with(ParameterSelection.nullSelection("@bag01", "@param"));
		performResourceQuery(query, Arrays.asList("@1", "@2", "@3"));
	}

	@Test
	public void shouldQueryResourceByBag() throws SQLException {
		ResourceQuery<Resource> query = ResourceQuery.query("MyType1", new OrderById());
		query.and().with(new ParameterBagSelection("@bag01"));
		performResourceQuery(query, Arrays.asList("@1", "@2", "@3"));
	}

	@Test
	public void shouldQueryResourceByNullBag() throws SQLException {
		ResourceQuery<Resource> query = ResourceQuery.query("MyType1", new OrderById());
		query.and().with(new NullParameterBagSelection("@bag01"));
		performResourceQuery(query, Arrays.<String> asList());
	}

}
