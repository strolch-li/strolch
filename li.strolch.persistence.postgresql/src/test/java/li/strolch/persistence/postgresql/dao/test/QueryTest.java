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
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import li.strolch.model.Order;
import li.strolch.model.Resource;
import li.strolch.model.activity.Activity;
import li.strolch.model.query.ActivityQuery;
import li.strolch.model.query.OrderQuery;
import li.strolch.model.query.ResourceQuery;
import li.strolch.persistence.postgresql.PostgreSqlActivityQueryVisitor;
import li.strolch.persistence.postgresql.PostgreSqlOrderQueryVisitor;
import li.strolch.persistence.postgresql.PostgreSqlQueryVisitor;
import li.strolch.persistence.postgresql.PostgreSqlResourceQueryVisitor;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class QueryTest {

	protected static final Logger logger = LoggerFactory.getLogger(QueryTest.class);

	protected Connection openConn() throws SQLException {
		String url = "jdbc:postgresql://localhost/testdb";
		String username = "testuser";
		String password = "test";
		Connection connection = DriverManager.getConnection(url, username, password);
		connection.setAutoCommit(false);
		return connection;
	}

	protected void performOrderQuery(OrderQuery<Order> query, List<String> expected) throws SQLException {
		PostgreSqlOrderQueryVisitor visitor = new PostgreSqlOrderQueryVisitor("id");
		query.accept(visitor);
		List<String> ids = queryIds(visitor);
		assertEquals(expected, ids);
	}

	protected void performActivityQuery(ActivityQuery<Activity> query, List<String> expected) throws SQLException {
		PostgreSqlActivityQueryVisitor visitor = new PostgreSqlActivityQueryVisitor("id");
		query.accept(visitor);
		List<String> ids = queryIds(visitor);
		assertEquals(expected, ids);
	}

	protected void performResourceQuery(ResourceQuery<Resource> query, List<String> expected) throws SQLException {
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

				try (ResultSet rs = ps.executeQuery()) {
					while (rs.next()) {
						ids.add(rs.getString(1));
					}
				}
			}
		}

		return ids;
	}
}
