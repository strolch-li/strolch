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

import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;
import java.io.IOException;
import java.io.InputStream;
import java.sql.*;
import java.text.MessageFormat;
import java.util.Calendar;

import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import li.strolch.model.Order;
import li.strolch.model.Tags;
import li.strolch.model.json.OrderFromJsonVisitor;
import li.strolch.model.xml.SimpleStrolchElementListener;
import li.strolch.model.xml.XmlModelSaxReader;
import li.strolch.persistence.api.OrderDao;
import li.strolch.persistence.api.StrolchPersistenceException;
import li.strolch.persistence.api.TransactionResult;
import org.xml.sax.SAXException;

@SuppressWarnings("nls")
public class PostgreSqlOrderDao extends PostgresqlDao<Order> implements OrderDao {

	public static final String ORDERS = "orders";

	private static final String insertAsXmlSqlS = "insert into {0} (id, version, created_by, created_at, deleted, latest, name, type, state, date, asxml) values (?, ?, ?, ?, ?, ?, ?, ?, ?::order_state, ?, ?)";
	private static final String insertAsJsonSqlS = "insert into {0} (id, version, created_by, created_at, deleted, latest, name, type, state, date, asjson) values (?, ?, ?, ?, ?, ?, ?, ?, ?::order_state, ?, ?)";

	private static final String updateAsXmlSqlS = "update {0} set created_by = ?, created_at = ?, deleted = ?, latest = ?, name = ?, type = ?, state = ?::order_state, date = ?, asxml = ? where id = ? and version = ?";
	private static final String updateAsJsonSqlS = "update {0} set created_by = ?, created_at = ?, deleted = ?, latest = ?, name = ?, type = ?, state = ?::order_state, date = ?, asjson = ? where id = ? and version = ?";

	private static final String updateLatestSqlS = "update {0} SET latest = false WHERE id = ? AND version = ?";

	public PostgreSqlOrderDao(DataType dataType, Connection connection, TransactionResult txResult,
			boolean versioningEnabled) {
		super(dataType, connection, txResult, versioningEnabled);
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

	@Override
	protected Order parseFromJson(String id, String type, String json) {
		JsonObject jsonObject = new JsonParser().parse(json).getAsJsonObject();
		return new OrderFromJsonVisitor().visit(jsonObject);
	}

	@Override
	protected void internalSave(final Order order) {

		String sql;
		if (this.dataType == DataType.xml)
			sql = insertAsXmlSqlS;
		else if (this.dataType == DataType.json)
			sql = insertAsJsonSqlS;
		else
			throw new IllegalStateException("Unhandled DataType " + this.dataType);
		sql = MessageFormat.format(sql, getTableName());

		try (PreparedStatement preparedStatement = this.connection.prepareStatement(sql)) {

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

			SQLXML sqlxml = writeObject(preparedStatement, order, 11);

			try {
				int modCount = preparedStatement.executeUpdate();
				if (modCount != 1) {
					String msg = "Expected to save 1 element with id {0} but SQL statement modified {1} elements!";
					msg = MessageFormat.format(msg, order.getId(), modCount);
					throw new StrolchPersistenceException(msg);
				}
			} finally {
				if (sqlxml != null)
					sqlxml.free();
			}

		} catch (SQLException | SAXException e) {
			throw new StrolchPersistenceException(MessageFormat
					.format("Failed to insert Order {0} due to {1}", order.getVersion(), e.getLocalizedMessage()), e);
		}

		if (order.getVersion().isFirstVersion()) {
			return;
		}

		// and set the previous version to not be latest anymore
		sql = MessageFormat.format(updateLatestSqlS, getTableName());
		try (PreparedStatement preparedStatement = this.connection.prepareStatement(sql)) {

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
			throw new StrolchPersistenceException(MessageFormat
					.format("Failed to update previous version of Order {0} due to {1}", order.getVersion(),
							e.getLocalizedMessage()), e);
		}
	}

	@Override
	protected void internalUpdate(final Order order) {

		// with versioning we save a new object
		if (this.versioningEnabled) {
			internalSave(order);
			return;
		}

		// make sure is first version when versioning is not enabled
		if (!order.getVersion().isFirstVersion()) {
			throw new StrolchPersistenceException(MessageFormat
					.format("Versioning is not enabled, so version must always be 0 to perform an update, but it is {0}",
							order.getVersion()));
		}

		// and also not marked as deleted!
		if (order.getVersion().isDeleted()) {
			throw new StrolchPersistenceException(MessageFormat
					.format("Versioning is not enabled, so version can not be marked as deleted for {0}",
							order.getVersion()));
		}

		String sql;
		if (this.dataType == DataType.xml)
			sql = updateAsXmlSqlS;
		else if (this.dataType == DataType.json)
			sql = updateAsJsonSqlS;
		else
			throw new IllegalStateException("Unhandled DataType " + this.dataType);
		sql = MessageFormat.format(sql, getTableName());

		// now we update the existing object
		try (PreparedStatement preparedStatement = this.connection.prepareStatement(sql)) {

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

			SQLXML sqlxml = writeObject(preparedStatement, order, 9);

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
				if (sqlxml != null)
					sqlxml.free();
			}

		} catch (SQLException | SAXException e) {
			throw new StrolchPersistenceException(MessageFormat
					.format("Failed to update Order {0} due to {1}", order.getLocator(), e.getLocalizedMessage()), e);
		}
	}
}
