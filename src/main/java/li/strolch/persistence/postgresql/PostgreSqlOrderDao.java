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
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either exporders or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package li.strolch.persistence.postgresql;

import java.io.IOException;
import java.io.InputStream;
import java.sql.Date;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.SQLXML;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;

import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;
import javax.xml.transform.sax.SAXResult;

import li.strolch.model.Order;
import li.strolch.model.OrderVisitor;
import li.strolch.model.Tags;
import li.strolch.model.query.OrderQuery;
import li.strolch.model.xml.OrderToSaxVisitor;
import li.strolch.model.xml.SimpleStrolchElementListener;
import li.strolch.model.xml.XmlModelSaxReader;
import li.strolch.persistence.api.OrderDao;
import li.strolch.persistence.api.StrolchPersistenceException;

import org.xml.sax.ContentHandler;
import org.xml.sax.SAXException;

@SuppressWarnings("nls")
public class PostgreSqlOrderDao extends PostgresqlDao<Order> implements OrderDao {

	public static final String ORDERS = "orders";

	/**
	 * @param tx
	 */
	public PostgreSqlOrderDao(PostgreSqlStrolchTransaction tx) {
		super(tx);
	}

	@Override
	protected String getClassName() {
		return Tags.ORDER;
	}

	@Override
	protected String getTableName() {
		return ORDERS;
	}

	@Override
	protected Order parseFromXml(String id, String type, SQLXML sqlxml) {
		SimpleStrolchElementListener listener = new SimpleStrolchElementListener();
		try (InputStream binaryStream = sqlxml.getBinaryStream()) {
			SAXParser parser = SAXParserFactory.newInstance().newSAXParser();
			parser.parse(binaryStream, new XmlModelSaxReader(listener));
		} catch (SQLException | IOException | SAXException | ParserConfigurationException e) {
			throw new StrolchPersistenceException(MessageFormat.format(
					"Failed to extract Order from sqlxml value for {0} / {1}", id, type));
		}

		if (listener.getOrders().size() == 0)
			throw new StrolchPersistenceException(MessageFormat.format(
					"No Orders parsed from sqlxml value for {0} / {1}", id, type));
		if (listener.getOrders().size() > 1)
			throw new StrolchPersistenceException(MessageFormat.format(
					"Multiple Orders parsed from sqlxml value for {0} / {1}", id, type));

		return listener.getOrders().get(0);
	}

	protected SQLXML createSqlXml(Order order, PreparedStatement preparedStatement) throws SQLException, SAXException {
		SQLXML sqlxml = this.tx.getConnection().createSQLXML();
		SAXResult saxResult = sqlxml.setResult(SAXResult.class);
		ContentHandler contentHandler = saxResult.getHandler();
		contentHandler.startDocument();
		new OrderToSaxVisitor(contentHandler).visit(order);
		contentHandler.endDocument();
		return sqlxml;
	}

	@Override
	protected void internalSave(final Order order) {
		String sql = "insert into " + getTableName()
				+ " (id, name, type, state, date, asxml) values (?, ?, ?, ?::order_state, ?, ?)";
		try (PreparedStatement preparedStatement = PostgreSqlOrderDao.this.tx.getConnection().prepareStatement(sql)) {
			preparedStatement.setString(1, order.getId());
			preparedStatement.setString(2, order.getName());
			preparedStatement.setString(3, order.getType());
			preparedStatement.setString(4, order.getState().name());
			preparedStatement.setDate(5, new Date(order.getDate().getTime()), Calendar.getInstance());

			SQLXML sqlxml = createSqlXml(order, preparedStatement);
			preparedStatement.setSQLXML(6, sqlxml);
			try {
				int modCount = preparedStatement.executeUpdate();
				if (modCount != 1) {
					String msg = "Expected to save 1 element with id {0} but SQL statement modified {1} elements!";
					msg = MessageFormat.format(msg, order.getId(), modCount);
					throw new StrolchPersistenceException(msg);
				}
			} finally {
				sqlxml.free();
			}

		} catch (SQLException | SAXException e) {
			throw new StrolchPersistenceException(MessageFormat.format("Failed to insert Order {0} due to {1}",
					order.getLocator(), e.getLocalizedMessage()), e);
		}
	}

	@Override
	protected void internalUpdate(final Order order) {
		String sql = "update " + getTableName()
				+ " set name = ?, type = ?, state = ?::order_state, date = ?, asxml = ? where id = ? ";
		try (PreparedStatement preparedStatement = PostgreSqlOrderDao.this.tx.getConnection().prepareStatement(sql)) {

			preparedStatement.setString(1, order.getName());
			preparedStatement.setString(2, order.getType());
			preparedStatement.setString(3, order.getState().name());
			preparedStatement.setDate(4, new Date(order.getDate().getTime()), Calendar.getInstance());
			preparedStatement.setString(6, order.getId());

			SQLXML sqlxml = createSqlXml(order, preparedStatement);
			preparedStatement.setSQLXML(5, sqlxml);
			try {
				int modCount = preparedStatement.executeUpdate();
				if (modCount != 1) {
					String msg = "Expected to update 1 element with id {0} but SQL statement modified {1} elements!";
					msg = MessageFormat.format(msg, order.getId(), modCount);
					throw new StrolchPersistenceException(msg);
				}
			} finally {
				sqlxml.free();
			}

		} catch (SQLException | SAXException e) {
			throw new StrolchPersistenceException(MessageFormat.format("Failed to update Order {0} due to {1}",
					order.getLocator(), e.getLocalizedMessage()), e);
		}
	}

	@Override
	public <U> List<U> doQuery(OrderQuery query, OrderVisitor<U> orderVisitor) {

		PostgreSqlOrderQueryVisitor queryVisitor = new PostgreSqlOrderQueryVisitor("id, asxml");
		query.accept(queryVisitor);
		queryVisitor.validate();

		List<U> list = new ArrayList<>();

		String sql = queryVisitor.getSql();
		try (PreparedStatement ps = PostgreSqlOrderDao.this.tx.getConnection().prepareStatement(sql)) {
			queryVisitor.setValues(ps);

			try (ResultSet result = ps.executeQuery()) {
				while (result.next()) {
					String id = result.getString("id");
					SQLXML sqlxml = result.getSQLXML("asxml");
					Order t = parseFromXml(id, queryVisitor.getType(), sqlxml);
					list.add(orderVisitor.visit(t));
				}
			}
		} catch (SQLException e) {
			throw new StrolchPersistenceException("Failed to perform query due to: " + e.getMessage(), e);
		}

		return list;
	}
}
