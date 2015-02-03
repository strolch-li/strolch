/*
 * Copyright 2013 Robert von Burg <eitch@eitchnet.ch>
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *     http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package li.strolch.persistence.postgresql.dao.test;

import static li.strolch.persistence.postgresql.dao.test.CachedDaoTest.CONFIG_SRC;
import static li.strolch.persistence.postgresql.dao.test.CachedDaoTest.DB_PASSWORD;
import static li.strolch.persistence.postgresql.dao.test.CachedDaoTest.DB_STORE_PATH_DIR;
import static li.strolch.persistence.postgresql.dao.test.CachedDaoTest.DB_URL;
import static li.strolch.persistence.postgresql.dao.test.CachedDaoTest.DB_USERNAME;
import static li.strolch.persistence.postgresql.dao.test.CachedDaoTest.RUNTIME_PATH;
import static li.strolch.persistence.postgresql.dao.test.CachedDaoTest.dropSchema;
import static org.junit.Assert.assertEquals;

import java.io.File;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.HashSet;
import java.util.List;

import li.strolch.agent.api.OrderMap;
import li.strolch.agent.api.ResourceMap;
import li.strolch.agent.api.StrolchRealm;
import li.strolch.model.ModelGenerator;
import li.strolch.model.State;
import li.strolch.model.query.DateSelection;
import li.strolch.model.query.IdSelection;
import li.strolch.model.query.NameSelection;
import li.strolch.model.query.OrSelection;
import li.strolch.model.query.OrderQuery;
import li.strolch.model.query.ParameterBagSelection;
import li.strolch.model.query.ParameterBagSelection.NullParameterBagSelection;
import li.strolch.model.query.ParameterSelection;
import li.strolch.model.query.ResourceQuery;
import li.strolch.model.query.StateSelection;
import li.strolch.model.query.StrolchTypeNavigation;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.persistence.postgresql.PostgreSqlOrderQueryVisitor;
import li.strolch.persistence.postgresql.PostgreSqlQueryVisitor;
import li.strolch.persistence.postgresql.PostgreSqlResourceQueryVisitor;
import li.strolch.runtime.StrolchConstants;
import li.strolch.testbase.runtime.RuntimeMock;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ch.eitchnet.privilege.model.Certificate;
import ch.eitchnet.utils.StringMatchMode;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class QueryTest {

	private static final Logger logger = LoggerFactory.getLogger(QueryTest.class);
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

		Certificate cert = runtimeMock.getPrivilegeHandler().authenticate("test", "test".getBytes());
		StrolchRealm realm = runtimeMock.getRealm(StrolchConstants.DEFAULT_REALM);
		try (StrolchTransaction tx = realm.openTx(cert, "test")) {
			OrderMap orderMap = tx.getOrderMap();

			orderMap.add(tx, ModelGenerator.createOrder("@1", "Order 1", "MyType1", earlier, State.CREATED));
			orderMap.add(tx, ModelGenerator.createOrder("@2", "Order 2", "MyType1", current, State.OPEN));
			orderMap.add(tx, ModelGenerator.createOrder("@3", "Order 3", "MyType1", later, State.CLOSED));
			orderMap.add(tx, ModelGenerator.createOrder("@4", "Order 4", "MyType2", earlier, State.CREATED));
			orderMap.add(tx, ModelGenerator.createOrder("@5", "Order 5", "MyType2", current, State.OPEN));
			orderMap.add(tx, ModelGenerator.createOrder("@6", "Order 6", "MyType2", later, State.CLOSED));

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

		OrderQuery query = new OrderQuery(new StrolchTypeNavigation("MyType1"));
		query.withAny();
		performOrderQuery(query, Arrays.asList("@1", "@2", "@3"));
	}

	@Test
	public void shouldQueryResourceAll() throws SQLException {

		ResourceQuery query = new ResourceQuery(new StrolchTypeNavigation("MyType2"));
		query.withAny();
		performResourceQuery(query, Arrays.asList("@4", "@5", "@6"));
	}

	@Test
	public void shouldQueryOrderByDate() throws SQLException {

		// range
		OrderQuery query = new OrderQuery(new StrolchTypeNavigation("MyType1"));
		query.and().with(new DateSelection().from(earlier, false).to(later, false));
		performOrderQuery(query, Arrays.asList("@1", "@2", "@3"));

		// equals current
		query = new OrderQuery(new StrolchTypeNavigation("MyType1"));
		query.and().with(new DateSelection().from(current, false).to(current, false));
		performOrderQuery(query, Arrays.asList("@2"));

		// equals later
		query = new OrderQuery(new StrolchTypeNavigation("MyType1"));
		query.and().with(new DateSelection().from(later, false).to(later, false));
		performOrderQuery(query, Arrays.<String> asList("@3"));

		// equals earlier
		query = new OrderQuery(new StrolchTypeNavigation("MyType1"));
		query.and().with(new DateSelection().from(earlier, false).to(earlier, false));
		performOrderQuery(query, Arrays.<String> asList("@1"));

		// past
		query = new OrderQuery(new StrolchTypeNavigation("MyType1"));
		query.and().with(new DateSelection().to(past, false));
		performOrderQuery(query, Arrays.<String> asList());

		// future
		query = new OrderQuery(new StrolchTypeNavigation("MyType1"));
		query.and().with(new DateSelection().from(future, false));
		performOrderQuery(query, Arrays.<String> asList());

		// earlier
		query = new OrderQuery(new StrolchTypeNavigation("MyType1"));
		query.and().with(new DateSelection().from(past, false).to(earlier, true));
		performOrderQuery(query, Arrays.<String> asList("@1"));

		// later
		query = new OrderQuery(new StrolchTypeNavigation("MyType1"));
		query.and().with(new DateSelection().from(later, false).to(future, true));
		performOrderQuery(query, Arrays.<String> asList("@3"));
	}

	@Test
	public void shouldQueryOrderByState() throws SQLException {

		OrderQuery query = new OrderQuery(new StrolchTypeNavigation("MyType1"));
		query.and().with(new StateSelection(State.CREATED));
		performOrderQuery(query, Arrays.asList("@1"));

		query = new OrderQuery(new StrolchTypeNavigation("MyType1"));
		query.and().with(new StateSelection(State.OPEN));
		performOrderQuery(query, Arrays.<String> asList("@2"));
	}

	@Test
	public void shouldQueryOrder1() throws SQLException {

		OrderQuery query = new OrderQuery(new StrolchTypeNavigation("MyType1"));
		query.and().with(new IdSelection("@1", "@2"),
				new NameSelection("Order 1", StringMatchMode.EQUALS_CASE_SENSITIVE));
		performOrderQuery(query, Arrays.asList("@1"));
	}

	@Test
	public void shouldQueryOrder2() throws SQLException {

		OrderQuery query = new OrderQuery(new StrolchTypeNavigation("MyType1"));
		query.or().with(new IdSelection("@1", "@2"),
				new NameSelection("order 1", StringMatchMode.EQUALS_CASE_SENSITIVE));
		performOrderQuery(query, Arrays.asList("@1", "@2"));
	}
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	@Test
	public void shouldQueryOrderByBooleParam() throws SQLException {
		OrderQuery query = new OrderQuery(new StrolchTypeNavigation("MyType1"));
		query.and().with(ParameterSelection.booleanSelection("@bag01", "@param1", true));
		performOrderQuery(query, Arrays.asList("@1", "@2", "@3"));
	}

	@Test
	public void shouldQueryOrderByFloagParam() throws SQLException {
		OrderQuery query = new OrderQuery(new StrolchTypeNavigation("MyType1"));
		query.and().with(ParameterSelection.floatSelection("@bag01", "@param2", 44.3));
		performOrderQuery(query, Arrays.asList("@1", "@2", "@3"));
	}

	@Test
	public void shouldQueryOrderByIntegerParam() throws SQLException {
		OrderQuery query = new OrderQuery(new StrolchTypeNavigation("MyType1"));
		query.and().with(ParameterSelection.integerSelection("@bag01", "@param3", 77));
		performOrderQuery(query, Arrays.asList("@1", "@2", "@3"));
	}

	@Test
	public void shouldQueryOrderByLongParam() throws SQLException {
		OrderQuery query = new OrderQuery(new StrolchTypeNavigation("MyType2"));
		query.and().with(ParameterSelection.longSelection("@bag01", "@param4", 4453234566L));
		performOrderQuery(query, Arrays.asList("@4", "@5", "@6"));
	}

	@Test
	public void shouldQueryOrderByStringParam() throws SQLException {

		List<String> expected = Arrays.asList("@1", "@2", "@3");

		OrderQuery query = new OrderQuery(new StrolchTypeNavigation("MyType1"));
		query.and().with(
				ParameterSelection.stringSelection("@bag01", "@param5", "Strolch",
						StringMatchMode.EQUALS_CASE_SENSITIVE));
		performOrderQuery(query, expected);

		query = new OrderQuery(new StrolchTypeNavigation("MyType1"));
		query.and().with(
				ParameterSelection.stringSelection("@bag01", "@param5", "strolch",
						StringMatchMode.EQUALS_CASE_SENSITIVE));
		performOrderQuery(query, Arrays.<String> asList());

		query = new OrderQuery(new StrolchTypeNavigation("MyType1"));
		query.and().with(
				ParameterSelection.stringSelection("@bag01", "@param5", "strolch",
						StringMatchMode.EQUALS_CASE_INSENSITIVE));
		performOrderQuery(query, expected);

		query = new OrderQuery(new StrolchTypeNavigation("MyType1"));
		query.and().with(
				ParameterSelection.stringSelection("@bag01", "@param5", "olch",
						StringMatchMode.CONTAINS_CASE_INSENSITIVE));
		performOrderQuery(query, expected);
	}

	@Test
	public void shouldQueryOrderByDateParam() throws SQLException {
		OrderQuery query = new OrderQuery(new StrolchTypeNavigation("MyType1"));
		query.and().with(ParameterSelection.dateSelection("@bag01", "@param6", new Date(1354295525628L)));
		performOrderQuery(query, Arrays.asList("@1", "@2", "@3"));
	}

	@Test
	public void shouldQueryOrderByDurationParam() throws SQLException {
		OrderQuery query = new OrderQuery(new StrolchTypeNavigation("MyType1"));
		query.and().with(ParameterSelection.durationSelection("@bag01", "@param8", "P1D"));
		performOrderQuery(query, Arrays.asList("@1", "@2", "@3"));
	}

	@Test
	public void shouldQueryOrderByNullParam1() throws SQLException {
		OrderQuery query = new OrderQuery(new StrolchTypeNavigation("MyType1"));
		query.and().with(ParameterSelection.nullSelection("@bag01", "@param6"));
		performOrderQuery(query, Arrays.<String> asList());
	}

	@Test
	public void shouldQueryOrderByNullParam2() throws SQLException {
		OrderQuery query = new OrderQuery(new StrolchTypeNavigation("MyType1"));
		query.and().with(ParameterSelection.nullSelection("@bag01", "@param"));
		performOrderQuery(query, Arrays.asList("@1", "@2", "@3"));
	}

	@Test
	public void shouldQueryOrderByBag() throws SQLException {
		OrderQuery query = new OrderQuery(new StrolchTypeNavigation("MyType1"));
		query.and().with(new ParameterBagSelection("@bag01"));
		performOrderQuery(query, Arrays.asList("@1", "@2", "@3"));
	}

	@Test
	public void shouldQueryOrderByNullBag() throws SQLException {
		OrderQuery query = new OrderQuery(new StrolchTypeNavigation("MyType1"));
		query.and().with(new NullParameterBagSelection("@bag01"));
		performOrderQuery(query, Arrays.<String> asList());
	}
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	

	@Test
	public void shouldQueryResource1() throws SQLException {

		ResourceQuery query = new ResourceQuery(new StrolchTypeNavigation("MyType1"));
		query.or().with(new IdSelection("@1", "@2"),
				new NameSelection("Resource 1", StringMatchMode.EQUALS_CASE_SENSITIVE));
		performResourceQuery(query, Arrays.asList("@1", "@2"));
	}

	@Test
	public void shouldQueryResource2() throws SQLException {
		ResourceQuery query = new ResourceQuery(new StrolchTypeNavigation("MyType1"));
		query.and().with(
				new OrSelection(new IdSelection("@1"), new IdSelection("@2")),
				new OrSelection(new NameSelection("Resource 1", StringMatchMode.EQUALS_CASE_SENSITIVE),
						new NameSelection("Resource 2", StringMatchMode.EQUALS_CASE_SENSITIVE)));
		performResourceQuery(query, Arrays.asList("@1", "@2"));
	}

	@Test
	public void shouldQueryResourceByBooleParam() throws SQLException {
		ResourceQuery query = new ResourceQuery(new StrolchTypeNavigation("MyType1"));
		query.and().with(ParameterSelection.booleanSelection("@bag01", "@param1", true));
		performResourceQuery(query, Arrays.asList("@1", "@2", "@3"));
	}

	@Test
	public void shouldQueryResourceByFloagParam() throws SQLException {
		ResourceQuery query = new ResourceQuery(new StrolchTypeNavigation("MyType1"));
		query.and().with(ParameterSelection.floatSelection("@bag01", "@param2", 44.3));
		performResourceQuery(query, Arrays.asList("@1", "@2", "@3"));
	}

	@Test
	public void shouldQueryResourceByIntegerParam() throws SQLException {
		ResourceQuery query = new ResourceQuery(new StrolchTypeNavigation("MyType1"));
		query.and().with(ParameterSelection.integerSelection("@bag01", "@param3", 77));
		performResourceQuery(query, Arrays.asList("@1", "@2", "@3"));
	}

	@Test
	public void shouldQueryResourceByLongParam() throws SQLException {
		ResourceQuery query = new ResourceQuery(new StrolchTypeNavigation("MyType2"));
		query.and().with(ParameterSelection.longSelection("@bag01", "@param4", 4453234566L));
		performResourceQuery(query, Arrays.asList("@4", "@5", "@6"));
	}

	@Test
	public void shouldQueryResourceByStringParam() throws SQLException {

		List<String> expected = Arrays.asList("@1", "@2", "@3");

		ResourceQuery query = new ResourceQuery(new StrolchTypeNavigation("MyType1"));
		query.and().with(
				ParameterSelection.stringSelection("@bag01", "@param5", "Strolch",
						StringMatchMode.EQUALS_CASE_SENSITIVE));
		performResourceQuery(query, expected);

		query = new ResourceQuery(new StrolchTypeNavigation("MyType1"));
		query.and().with(
				ParameterSelection.stringSelection("@bag01", "@param5", "strolch",
						StringMatchMode.EQUALS_CASE_SENSITIVE));
		performResourceQuery(query, Arrays.<String> asList());

		query = new ResourceQuery(new StrolchTypeNavigation("MyType1"));
		query.and().with(
				ParameterSelection.stringSelection("@bag01", "@param5", "strolch",
						StringMatchMode.EQUALS_CASE_INSENSITIVE));
		performResourceQuery(query, expected);

		query = new ResourceQuery(new StrolchTypeNavigation("MyType1"));
		query.and().with(
				ParameterSelection.stringSelection("@bag01", "@param5", "olch",
						StringMatchMode.CONTAINS_CASE_INSENSITIVE));
		performResourceQuery(query, expected);
	}

	@Test
	public void shouldQueryResourceByDateParam() throws SQLException {
		ResourceQuery query = new ResourceQuery(new StrolchTypeNavigation("MyType1"));
		query.and().with(ParameterSelection.dateSelection("@bag01", "@param6", new Date(1354295525628L)));
		performResourceQuery(query, Arrays.asList("@1", "@2", "@3"));
	}

	@Test
	public void shouldQueryResourceByDurationParam() throws SQLException {
		ResourceQuery query = new ResourceQuery(new StrolchTypeNavigation("MyType1"));
		query.and().with(ParameterSelection.durationSelection("@bag01", "@param8", "P1D"));
		performResourceQuery(query, Arrays.asList("@1", "@2", "@3"));
	}

	@Test
	public void shouldQueryResourceByNullParam1() throws SQLException {
		ResourceQuery query = new ResourceQuery(new StrolchTypeNavigation("MyType1"));
		query.and().with(ParameterSelection.nullSelection("@bag01", "@param6"));
		performResourceQuery(query, Arrays.<String> asList());
	}

	@Test
	public void shouldQueryResourceByNullParam2() throws SQLException {
		ResourceQuery query = new ResourceQuery(new StrolchTypeNavigation("MyType1"));
		query.and().with(ParameterSelection.nullSelection("@bag01", "@param"));
		performResourceQuery(query, Arrays.asList("@1", "@2", "@3"));
	}

	@Test
	public void shouldQueryResourceByBag() throws SQLException {
		ResourceQuery query = new ResourceQuery(new StrolchTypeNavigation("MyType1"));
		query.and().with(new ParameterBagSelection("@bag01"));
		performResourceQuery(query, Arrays.asList("@1", "@2", "@3"));
	}

	@Test
	public void shouldQueryResourceByNullBag() throws SQLException {
		ResourceQuery query = new ResourceQuery(new StrolchTypeNavigation("MyType1"));
		query.and().with(new NullParameterBagSelection("@bag01"));
		performResourceQuery(query, Arrays.<String> asList());
	}

	private void performOrderQuery(OrderQuery query, List<String> expected) throws SQLException {
		PostgreSqlOrderQueryVisitor visitor = new PostgreSqlOrderQueryVisitor("id");
		query.accept(visitor);
		List<String> ids = queryIds(visitor);
		assertEquals(new HashSet<>(expected), new HashSet<>(ids));
	}

	private void performResourceQuery(ResourceQuery query, List<String> expected) throws SQLException {
		PostgreSqlResourceQueryVisitor visitor = new PostgreSqlResourceQueryVisitor("id");
		query.accept(visitor);
		List<String> ids = queryIds(visitor);
		assertEquals(new HashSet<>(expected), new HashSet<>(ids));
	}

	private List<String> queryIds(PostgreSqlQueryVisitor visitor) throws SQLException {
		String sql = visitor.getSql();
		logger.info("\n" + sql);
		List<String> ids = new ArrayList<>();
		try (Connection con = openConn()) {
			try (PreparedStatement ps = con.prepareStatement(sql)) {
				visitor.setValues(ps);

				ResultSet rs = ps.executeQuery();
				while (rs.next()) {
					ids.add(rs.getString(1));
				}
			}
		}

		return ids;
	}
}
