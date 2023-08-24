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
import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;

import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import li.strolch.model.Order;
import li.strolch.model.json.OrderFromJsonVisitor;
import li.strolch.model.xml.SimpleStrolchElementListener;
import li.strolch.model.xml.XmlModelSaxReader;
import li.strolch.persistence.api.OrderDao;
import li.strolch.persistence.api.StrolchPersistenceException;
import li.strolch.persistence.api.TransactionResult;
import li.strolch.utils.collections.DateRange;
import li.strolch.utils.iso8601.ISO8601;
import org.xml.sax.SAXException;

@SuppressWarnings("nls")
public class PostgreSqlOrderDao extends PostgresqlDao<Order> implements OrderDao {

	public static final String ORDERS = "orders";

	private static final String querySizeDrSqlS = "select count(*) from {0} where latest = true {1}";
	private static final String querySizeOfTypeDrSqlS = "select count(*) from {0} where type = ANY(?) and latest = true {1}";

	private static final String queryAllDrAsXmlSqlS = "select id, type, asxml from {0} where latest = true {1}";
	private static final String queryAllDrAsXmlLimitSqlS = "select id, type, asxml from {0} where latest = true {1} order by date {2} limit {3} offset {4}";
	private static final String queryAllDrAsJsonSqlS = "select id, type, asjson from {0} where latest = true {1}";
	private static final String queryAllDrAsJsonLimitSqlS = "select id, type, asjson from {0} where latest = true {1} order by date {2} limit {3} offset {4}";

	private static final String queryAllByTypeDrAsXmlSqlS = "select id, type, asxml from {0} where type = ANY(?) and latest = true {1}";
	private static final String queryAllByTypeDrAsXmlLimitSqlS = "select id, type, asxml from {0} where type = ANY(?) and latest = true {1} order by date {2} limit {3,number,#} offset {4,number,#}";
	private static final String queryAllByTypeDrAsJsonSqlS = "select id, type, asjson from {0} where type = ANY(?) and latest = true {1}";
	private static final String queryAllByTypeDrAsJsonLimitSqlS = "select id, type, asjson from {0} where type = ANY(?) and latest = true {1} order by date {2} limit {3,number,#} offset {4,number,#}";

	private static final String insertAsXmlSqlS = "insert into {0} (id, version, created_by, created_at, updated_at, deleted, latest, name, type, state, date, asxml) values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?::order_state, ?, ?)";
	private static final String insertAsJsonSqlS = "insert into {0} (id, version, created_by, created_at, updated_at, deleted, latest, name, type, state, date, asjson) values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?::order_state, ?, ?)";

	private static final String updateAsXmlSqlS = "update {0} set created_by = ?, created_at = ?, updated_at = ?, deleted = ?, latest = ?, name = ?, state = ?::order_state, date = ?, asxml = ? where type = ? and id = ? and version = ?";
	private static final String updateAsJsonSqlS = "update {0} set created_by = ?, created_at = ?, updated_at = ?, deleted = ?, latest = ?, name = ?, state = ?::order_state, date = ?, asjson = ? where type = ? and id = ? and version = ?";

	private static final String updateLatestSqlS = "update {0} SET latest = false WHERE type = ? and id = ? AND version = ?";

	public PostgreSqlOrderDao(DataType dataType, Connection connection, TransactionResult txResult,
			boolean versioningEnabled) {
		super(dataType, connection, txResult, versioningEnabled);
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

		if (listener.getOrders().isEmpty())
			throw new StrolchPersistenceException(
					MessageFormat.format("No Orders parsed from sqlxml value for {0} / {1}", id, type));
		if (listener.getOrders().size() > 1)
			throw new StrolchPersistenceException(
					MessageFormat.format("Multiple Orders parsed from sqlxml value for {0} / {1}", id, type));

		return listener.getOrders().get(0);
	}

	@Override
	protected Order parseFromJson(String id, String type, String json) {
		JsonObject jsonObject = JsonParser.parseString(json).getAsJsonObject();
		return new OrderFromJsonVisitor().visit(jsonObject);
	}

	@Override
	protected void internalSave(final Order order) {

		String sql = getSql(insertAsXmlSqlS, insertAsJsonSqlS);

		try (PreparedStatement preparedStatement = this.connection.prepareStatement(sql)) {

			// id
			preparedStatement.setString(1, order.getId());

			// version
			preparedStatement.setInt(2, order.getVersion().getVersion());
			preparedStatement.setString(3, order.getVersion().getCreatedBy());
			preparedStatement
					.setTimestamp(4, new Timestamp(order.getVersion().getCreated().getTime()), Calendar.getInstance());
			preparedStatement
					.setTimestamp(5, new Timestamp(order.getVersion().getUpdated().getTime()), Calendar.getInstance());
			preparedStatement.setBoolean(6, order.getVersion().isDeleted());

			preparedStatement.setBoolean(7, !order.getVersion().isDeleted());

			// attributes
			preparedStatement.setString(8, order.getName());
			preparedStatement.setString(9, order.getType());
			preparedStatement.setString(10, order.getState().name());
			preparedStatement.setTimestamp(11, new Timestamp(order.getDate().getTime()), Calendar.getInstance());

			SQLXML sqlxml = writeObject(preparedStatement, order, 12);

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
			preparedStatement.setString(1, order.getType());
			preparedStatement.setString(2, order.getId());
			preparedStatement.setInt(3, order.getVersion().getPreviousVersion());

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

		String sql = getSql(updateAsXmlSqlS, updateAsJsonSqlS);

		// now we update the existing object
		try (PreparedStatement preparedStatement = this.connection.prepareStatement(sql)) {

			// version
			preparedStatement.setString(1, order.getVersion().getCreatedBy());
			preparedStatement
					.setTimestamp(2, new Timestamp(order.getVersion().getCreated().getTime()), Calendar.getInstance());
			preparedStatement
					.setTimestamp(3, new Timestamp(order.getVersion().getUpdated().getTime()), Calendar.getInstance());
			preparedStatement.setBoolean(4, order.getVersion().isDeleted());

			preparedStatement.setBoolean(5, !order.getVersion().isDeleted());

			// attributes
			preparedStatement.setString(6, order.getName());
			preparedStatement.setString(7, order.getState().name());
			preparedStatement.setTimestamp(8, new Timestamp(order.getDate().getTime()), Calendar.getInstance());

			SQLXML sqlxml = writeObject(preparedStatement, order, 9);

			// primary key
			preparedStatement.setString(10, order.getType());
			preparedStatement.setString(11, order.getId());
			preparedStatement.setInt(12, order.getVersion().getVersion());

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

	@Override
	public long querySize(DateRange dateRange) {
		String sql = MessageFormat.format(querySizeDrSqlS, getTableName(), buildDateRangeClaus(dateRange));
		try (PreparedStatement statement = this.connection.prepareStatement(sql)) {

			try (ResultSet result = statement.executeQuery()) {
				result.next();
				return result.getLong(1);
			}

		} catch (SQLException e) {
			throw new StrolchPersistenceException("Failed to query size due to: " + e.getMessage(), e);
		}
	}

	@Override
	public long querySize(DateRange dateRange, String... types) {
		if (types.length == 0)
			return querySize();

		String sql = MessageFormat.format(querySizeOfTypeDrSqlS, getTableName(), buildDateRangeClaus(dateRange));

		try (PreparedStatement statement = this.connection.prepareStatement(sql)) {

			Array typesArray = statement.getConnection().createArrayOf("varchar", types);
			statement.setArray(1, typesArray);

			try (ResultSet result = statement.executeQuery()) {
				result.next();
				return result.getLong(1);
			}

		} catch (SQLException e) {
			throw new StrolchPersistenceException("Failed to query size due to: " + e.getMessage(), e);
		}
	}

	@Override
	public List<Order> queryAll(DateRange dateRange) throws StrolchPersistenceException {
		return queryAll(dateRange, Integer.MAX_VALUE, 0, true);
	}

	@Override
	public List<Order> queryAll(DateRange dateRange, long limit, long offset, boolean asc)
			throws StrolchPersistenceException {

		List<Order> list = new ArrayList<>();

		String sql = getLimitSql(dateRange, limit, offset, asc, queryAllDrAsXmlSqlS, queryAllDrAsJsonSqlS,
				queryAllDrAsXmlLimitSqlS, queryAllDrAsJsonLimitSqlS);

		try (PreparedStatement statement = this.connection.prepareStatement(sql)) {

			try (ResultSet result = statement.executeQuery()) {
				while (result.next()) {
					String id = result.getString("id");
					String type = result.getString("type");
					list.add(parseDbObject(result, id, type));
				}

				return list;
			}

		} catch (SQLException e) {
			throw new StrolchPersistenceException("Failed to query types due to: " + e.getMessage(), e);
		}
	}

	@Override
	public List<Order> queryAll(DateRange dateRange, String... types) throws StrolchPersistenceException {
		return queryAll(dateRange, Integer.MAX_VALUE, 0, true, types);
	}

	@Override
	public List<Order> queryAll(DateRange dateRange, long limit, long offset, boolean asc, String... types)
			throws StrolchPersistenceException {
		if (types.length == 0)
			return queryAll(limit, offset);

		List<Order> list = new ArrayList<>();

		String sql = getLimitSql(dateRange, limit, offset, asc, queryAllByTypeDrAsXmlSqlS, queryAllByTypeDrAsJsonSqlS,
				queryAllByTypeDrAsXmlLimitSqlS, queryAllByTypeDrAsJsonLimitSqlS);

		try (PreparedStatement statement = this.connection.prepareStatement(sql)) {

			Array typesArray = statement.getConnection().createArrayOf("varchar", types);
			statement.setArray(1, typesArray);

			try (ResultSet result = statement.executeQuery()) {
				while (result.next()) {
					String id = result.getString("id");
					String type = result.getString("type");
					list.add(parseDbObject(result, id, type));
				}

				return list;
			}

		} catch (SQLException e) {
			throw new StrolchPersistenceException("Failed to query types due to: " + e.getMessage(), e);
		}
	}

	protected String getLimitSql(DateRange dateRange, long limit, long offset, boolean asc, String xmlSql,
			String jsonSql, String xmlLimitSql, String jsonLimitSql) {

		String sql;
		if (limit == Integer.MAX_VALUE)
			return getSql(dateRange, xmlSql, jsonSql);

		if (this.dataType == DataType.xml)
			sql = xmlLimitSql;
		else if (this.dataType == DataType.json)
			sql = jsonLimitSql;
		else
			throw new IllegalStateException("Unhandled DataType " + this.dataType);

		String dateRangeClause = buildDateRangeClaus(dateRange);
		return MessageFormat.format(sql, getTableName(), dateRangeClause, asc ? "ASC" : "DESC", limit, offset);
	}

	protected String getSql(DateRange dateRange, String xmlSql, String jsonSql) {

		String sql;
		if (this.dataType == DataType.xml)
			sql = xmlSql;
		else if (this.dataType == DataType.json)
			sql = jsonSql;
		else
			throw new IllegalStateException("Unhandled DataType " + this.dataType);

		String dateRangeClause = buildDateRangeClaus(dateRange);
		return MessageFormat.format(sql, getTableName(), dateRangeClause);
	}

	private String buildDateRangeClaus(DateRange dateRange) {

		if (dateRange.isFromBounded() && dateRange.isToBounded()) {

			String from = ISO8601.toString(dateRange.getFromDate());
			String to = ISO8601.toString(dateRange.getToDate());

			if (dateRange.isFromInclusive() && dateRange.isToInclusive())
				return "and date >= '" + from + "' and date <= '" + to + "'";

			if (dateRange.isFromInclusive())
				return "and date >= '" + from + "' and date < '" + to + "'";

			if (dateRange.isToInclusive())
				return "and date > '" + from + "' and date <= '" + to + "'";

			return "and date > '" + from + "' and date < '" + to + "'";

		} else if (dateRange.isFromBounded()) {

			String from = ISO8601.toString(dateRange.getFromDate());

			if (dateRange.isFromInclusive())
				return "and date >= '" + from + "'";

			return "and date > '" + from + "'";

		} else if (dateRange.isToBounded()) {

			String to = ISO8601.toString(dateRange.getToDate());

			if (dateRange.isToInclusive())
				return "and date <= '" + to + "'";

			return "and date < '" + to + "'";

		}

		// no date range
		return "";
	}
}
