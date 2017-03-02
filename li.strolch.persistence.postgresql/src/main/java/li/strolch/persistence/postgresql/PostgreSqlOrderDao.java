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
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.SQLXML;
import java.sql.Timestamp;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;

import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;
import javax.xml.transform.sax.SAXResult;

import org.xml.sax.ContentHandler;
import org.xml.sax.SAXException;

import li.strolch.model.Order;
import li.strolch.model.Tags;
import li.strolch.model.query.OrderQuery;
import li.strolch.model.xml.SimpleStrolchElementListener;
import li.strolch.model.xml.StrolchElementToSaxVisitor;
import li.strolch.model.xml.XmlModelSaxReader;
import li.strolch.persistence.api.OrderDao;
import li.strolch.persistence.api.StrolchPersistenceException;

@SuppressWarnings("nls")
public class PostgreSqlOrderDao extends PostgresqlDao<Order> implements OrderDao {

	public static final String ORDERS = "orders";

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
			throw new StrolchPersistenceException(
					MessageFormat.format("Failed to extract Order from sqlxml value for {0} / {1}", id, type), e);
		}

		if (listener.getOrders().size() == 0)
			throw new StrolchPersistenceException(
					MessageFormat.format("No Orders parsed from sqlxml value for {0} / {1}", id, type));
		if (listener.getOrders().size() > 1)
			throw new StrolchPersistenceException(
					MessageFormat.format("Multiple Orders parsed from sqlxml value for {0} / {1}", id, type));

		return listener.getOrders().get(0);
	}

	protected SQLXML createSqlXml(Order order, PreparedStatement preparedStatement) throws SQLException, SAXException {
		SQLXML sqlxml = tx().getConnection().createSQLXML();
		SAXResult saxResult = sqlxml.setResult(SAXResult.class);
		ContentHandler contentHandler = saxResult.getHandler();
		contentHandler.startDocument();
		order.accept(new StrolchElementToSaxVisitor(contentHandler));
		contentHandler.endDocument();
		return sqlxml;
	}

	@Override
	protected void internalSave(final Order order) {

		String sql = "insert into " + getTableName()
				+ " (id, version, created_by, created_at, deleted, latest, name, type, state, date, asxml) values (?, ?, ?, ?, ?, ?, ?, ?, ?::order_state, ?, ?)";

		try (PreparedStatement preparedStatement = tx().getConnection().prepareStatement(sql)) {

			// id
			preparedStatement.setString(1, order.getId());

			// version
			preparedStatement.setInt(2, order.getVersion().getVersion());
			preparedStatement.setString(3, order.getVersion().getCreatedBy());
			preparedStatement.setTimestamp(4, new Timestamp(order.getVersion().getCreatedAt().getTime()),
					Calendar.getInstance());
			preparedStatement.setBoolean(5, order.getVersion().isDeleted());

			preparedStatement.setBoolean(6, !order.getVersion().isDeleted());

			// attributes
			preparedStatement.setString(7, order.getName());
			preparedStatement.setString(8, order.getType());
			preparedStatement.setString(9, order.getState().name());
			preparedStatement.setTimestamp(10, new Timestamp(order.getDate().getTime()), Calendar.getInstance());

			SQLXML sqlxml = createSqlXml(order, preparedStatement);
			preparedStatement.setSQLXML(11, sqlxml);

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
					order.getVersion(), e.getLocalizedMessage()), e);
		}

		if (order.getVersion().isFirstVersion()) {
			return;
		}

		// and set the previous version to not be latest anymore
		sql = "update " + getTableName() + " SET latest = false WHERE id = ? AND version = ?";
		try (PreparedStatement preparedStatement = tx().getConnection().prepareStatement(sql)) {

			// primary key
			preparedStatement.setString(1, order.getId());
			preparedStatement.setInt(2, order.getVersion().getPreviousVersion());

			int modCount = preparedStatement.executeUpdate();
			if (modCount != 1) {
				String msg = "Expected to update 1 previous element with id {0} and version {1} but SQL statement modified {2} elements!";
				msg = MessageFormat.format(msg, order.getId(), order.getVersion().getPreviousVersion(), modCount);
				throw new StrolchPersistenceException(msg);
			}

		} catch (SQLException e) {
			throw new StrolchPersistenceException(
					MessageFormat.format("Failed to update previous version of Order {0} due to {1}",
							order.getVersion(), e.getLocalizedMessage()),
					e);
		}
	}

	@Override
	protected void internalUpdate(final Order order) {

		// with versioning we save a new object
		if (tx().getRealm().isVersioningEnabled()) {
			internalSave(order);
			return;
		}

		// make sure is first version when versioning is not enabled
		if (!order.getVersion().isFirstVersion()) {
			throw new StrolchPersistenceException(MessageFormat.format(
					"Versioning is not enabled, so version must always be 0 to perform an update, but it is {0}",
					order.getVersion()));
		}

		// and also not marked as deleted!
		if (order.getVersion().isDeleted()) {
			throw new StrolchPersistenceException(MessageFormat.format(
					"Versioning is not enabled, so version can not be marked as deleted for {0}", order.getVersion()));
		}

		// now we update the existing object
		String sql = "update " + getTableName()
				+ " set created_by = ?, created_at = ?, deleted = ?, latest = ?, name = ?, type = ?, state = ?::order_state, date = ?, asxml = ? where id = ? and version = ?";
		try (PreparedStatement preparedStatement = tx().getConnection().prepareStatement(sql)) {

			// version
			preparedStatement.setString(1, order.getVersion().getCreatedBy());
			preparedStatement.setTimestamp(2, new Timestamp(order.getVersion().getCreatedAt().getTime()),
					Calendar.getInstance());
			preparedStatement.setBoolean(3, order.getVersion().isDeleted());

			preparedStatement.setBoolean(4, !order.getVersion().isDeleted());

			// attributes
			preparedStatement.setString(5, order.getName());
			preparedStatement.setString(6, order.getType());
			preparedStatement.setString(7, order.getState().name());
			preparedStatement.setTimestamp(8, new Timestamp(order.getDate().getTime()), Calendar.getInstance());

			SQLXML sqlxml = createSqlXml(order, preparedStatement);
			preparedStatement.setSQLXML(9, sqlxml);

			// primary key
			preparedStatement.setString(10, order.getId());
			preparedStatement.setInt(11, order.getVersion().getVersion());

			try {
				int modCount = preparedStatement.executeUpdate();
				if (modCount != 1) {
					String msg = "Expected to update 1 element with id {0} and version {1} but SQL statement modified {2} elements!";
					msg = MessageFormat.format(msg, order.getId(), order.getVersion().getVersion(), modCount);
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
	public <U> List<U> doQuery(OrderQuery<U> query) {

		PostgreSqlOrderQueryVisitor queryVisitor = new PostgreSqlOrderQueryVisitor("id, asxml");
		query.accept(queryVisitor);
		queryVisitor.validate();

		List<U> list = new ArrayList<>();

		String sql = queryVisitor.getSql();
		try (PreparedStatement ps = tx().getConnection().prepareStatement(sql)) {
			queryVisitor.setValues(ps);

			try (ResultSet result = ps.executeQuery()) {
				while (result.next()) {
					String id = result.getString("id");
					SQLXML sqlxml = result.getSQLXML("asxml");
					Order t = parseFromXml(id, queryVisitor.getType(), sqlxml);
					list.add(query.getOrderVisitor().visit(t));
				}
			}
		} catch (SQLException e) {
			throw new StrolchPersistenceException("Failed to perform query due to: " + e.getMessage(), e);
		}

		return list;
	}
}
