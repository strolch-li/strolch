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

import static org.junit.Assert.assertEquals;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

import li.strolch.model.query.IdSelection;
import li.strolch.model.query.NameSelection;
import li.strolch.model.query.OrSelection;
import li.strolch.model.query.OrderQuery;
import li.strolch.model.query.ParameterSelection;
import li.strolch.model.query.ResourceQuery;
import li.strolch.model.query.StrolchTypeNavigation;
import li.strolch.persistence.postgresql.PostgreSqlOrderQueryVisitor;
import li.strolch.persistence.postgresql.PostgreSqlQueryVisitor;
import li.strolch.persistence.postgresql.PostgreSqlResourceQueryVisitor;

import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ch.eitchnet.utils.StringMatchMode;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class QueryTest {

	private static final Logger logger = LoggerFactory.getLogger(QueryTest.class);

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

		OrderQuery query = new OrderQuery(new StrolchTypeNavigation("QTestType1"));
		query.withAny();
		performOrderQuery(query, Arrays.asList("myTestOrder1"));
	}

	@Test
	public void shouldQueryResourceAll() throws SQLException {

		ResourceQuery query = new ResourceQuery(new StrolchTypeNavigation("MyType1"));
		query.withAny();
		performResourceQuery(query, Arrays.asList("@_00000000", "@_00000001", "@_00000002", "@_00000003", "@_00000004"));
	}

	@Test
	public void shouldQueryOrder1() throws SQLException {

		OrderQuery query = new OrderQuery(new StrolchTypeNavigation("QTestType1"));
		query.and().with(new IdSelection("myTestOrder1", "@2"),
				new NameSelection("Test Name", StringMatchMode.EQUALS_CASE_SENSITIVE));
		performOrderQuery(query, Arrays.asList("myTestOrder1"));
	}

	@Test
	public void shouldQueryOrder2() throws SQLException {

		OrderQuery query = new OrderQuery(new StrolchTypeNavigation("QTestType1"));
		query.or().with(new IdSelection("myTestOrder1", "@2"),
				new NameSelection("Test Name", StringMatchMode.EQUALS_CASE_SENSITIVE));
		performOrderQuery(query, Arrays.asList("myTestOrder1"));
	}

	@Test
	public void shouldQueryResource1() throws SQLException {

		ResourceQuery query = new ResourceQuery(new StrolchTypeNavigation("MyType1"));
		query.or().with(new IdSelection("@_00000000", "@_00000001"),
				new NameSelection("Test Name", StringMatchMode.EQUALS_CASE_SENSITIVE));
		performResourceQuery(query, Arrays.asList("@_00000000", "@_00000001"));
	}

	@Test
	public void shouldQueryResource2() throws SQLException {
		ResourceQuery query = new ResourceQuery(new StrolchTypeNavigation("MyType1"));
		query.and().with(
				new OrSelection(new IdSelection("@_00000000"), new IdSelection("@_00000001")),
				new OrSelection(new NameSelection("My Resource  0", StringMatchMode.EQUALS_CASE_SENSITIVE),
						new NameSelection("My Resource  1", StringMatchMode.EQUALS_CASE_SENSITIVE)));
		performResourceQuery(query, Arrays.asList("@_00000000", "@_00000001"));
	}

	@Test
	public void shouldQueryResourceByBooleParam() throws SQLException {

		// select id, name, type, asxml 
		// from 
		//   resources 
		// where 
		//   type = 'MyType1' and 
		//   (
		//     cast(xpath('//Resource/ParameterBag/Parameter[@Id="@param1" and @Value="true"]', asxml) as text[]) != '{}'
		//   )

		ResourceQuery query = new ResourceQuery(new StrolchTypeNavigation("MyType1"));
		query.and().with(ParameterSelection.booleanSelection("@bag01", "@param1", true));
		performResourceQuery(query, Arrays.asList("@_00000000", "@_00000001", "@_00000002", "@_00000003", "@_00000004"));
	}

	@Test
	public void shouldQueryResourceByFloagParam() throws SQLException {
		ResourceQuery query = new ResourceQuery(new StrolchTypeNavigation("MyType1"));
		query.and().with(ParameterSelection.floatSelection("@bag01", "@param2", 44.3));
		performResourceQuery(query, Arrays.asList("@_00000000", "@_00000001", "@_00000002", "@_00000003", "@_00000004"));
	}

	@Test
	public void shouldQueryResourceByIntegerParam() throws SQLException {
		ResourceQuery query = new ResourceQuery(new StrolchTypeNavigation("MyType1"));
		query.and().with(ParameterSelection.integerSelection("@bag01", "@param3", 77));
		performResourceQuery(query, Arrays.asList("@_00000000", "@_00000001", "@_00000002", "@_00000003", "@_00000004"));
	}

	@Test
	public void shouldQueryResourceByLongParam() throws SQLException {
		ResourceQuery query = new ResourceQuery(new StrolchTypeNavigation("MyType1"));
		query.and().with(ParameterSelection.longSelection("@bag01", "@param4", 4453234566L));
		performResourceQuery(query, Arrays.asList("@_00000000", "@_00000001", "@_00000002", "@_00000003", "@_00000004"));
	}

	@Test
	public void shouldQueryResourceByStringParam() throws SQLException {

		List<String> expected = Arrays.asList("@_00000000", "@_00000001", "@_00000002", "@_00000003", "@_00000004");

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
		performResourceQuery(query, Arrays.asList("@_00000000", "@_00000001", "@_00000002", "@_00000003", "@_00000004"));

	}

	private void performOrderQuery(OrderQuery query, List<String> expected) throws SQLException {
		PostgreSqlOrderQueryVisitor visitor = new PostgreSqlOrderQueryVisitor("id");
		query.accept(visitor);
		List<String> ids = queryIds(visitor);
		assertEquals(expected, ids);
	}

	private void performResourceQuery(ResourceQuery query, List<String> expected) throws SQLException {
		PostgreSqlResourceQueryVisitor visitor = new PostgreSqlResourceQueryVisitor("id");
		query.accept(visitor);
		List<String> ids = queryIds(visitor);
		assertEquals(expected, ids);
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
