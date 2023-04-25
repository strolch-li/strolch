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
package li.strolch.persistence.postgresql;

import javax.xml.transform.sax.SAXResult;
import java.sql.*;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import li.strolch.model.StrolchRootElement;
import li.strolch.model.json.StrolchRootElementToJsonVisitor;
import li.strolch.model.xml.StrolchElementToSaxVisitor;
import li.strolch.persistence.api.StrolchDao;
import li.strolch.persistence.api.StrolchPersistenceException;
import li.strolch.persistence.api.TransactionResult;
import org.postgresql.util.PGobject;
import org.xml.sax.ContentHandler;
import org.xml.sax.SAXException;

public abstract class PostgresqlDao<T extends StrolchRootElement> implements StrolchDao<T> {

	private static final String querySizeSqlS = "select count(*) from {0} where latest = true";
	private static final String querySizeOfTypeSqlS = "select count(*) from {0} where type = ANY(?) and latest = true";
	private static final String querySizeOfElementSqlS = "select count(*) from {0} where type = ? and id = ?";
	private static final String queryTypesSqlS = "select distinct type from {0} where latest = true";
	private static final String queryLatestVersionNumberForSqlS = "select count(*), max(version) from {0} where type = ? and id = ?";
	private static final String queryVersionsSizeForSqlS = "select count(*) from {0} where type = ? and id = ?";

	private static final String updateLatestSqlS = "update {0} set latest = true where type = ? and id = ? and version = ?";

	private static final String deleteElementSqlS = "delete from {0} where id = ?";
	private static final String deleteVersionSqlS = "delete from {0} where type = ? and id = ? and version = ? and latest = true";
	private static final String deleteAllSqlS = "delete from {0}";
	private static final String deleteAllByTypeSqlS = "delete from {0} where type = ?";

	private static final String queryByVersionAsXmlSqlS = "select version, asxml from {0} where type = ? and id = ? and version = ?";
	private static final String queryByVersionAsJsonSqlS = "select version, asjson from {0} where type = ? and id = ? and version = ?";

	private static final String queryVersionsAsXmlForSqlS = "select asxml from {0} where type = ? and id = ? order by version";
	private static final String queryVersionsAsJsonForSqlS = "select asjson from {0} where type = ? and id = ? order by version";

	private static final String queryAllAsXmlSqlS = "select id, type, asxml from {0} where latest = true";
	private static final String queryAllAsXmlLimitSqlS = "select id, type, asxml from {0} where latest = true order by id limit {1} offset {2}";
	private static final String queryAllAsJsonSqlS = "select id, type, asjson from {0} where latest = true";
	private static final String queryAllAsJsonLimitSqlS = "select id, type, asjson from {0} where latest = true order by id limit {1} offset {2}";

	private static final String queryAllByTypeAsXmlSqlS = "select id, type, asxml from {0} where type = ANY(?) and latest = true";
	private static final String queryAllByTypeAsXmlLimitSqlS = "select id, type, asxml from {0} where type = ANY(?) and latest = true order by id limit {1,number,#} offset {2,number,#}";
	private static final String queryAllByTypeAsJsonSqlS = "select id, type, asjson from {0} where type = ANY(?) and latest = true";
	private static final String queryAllByTypeAsJsonLimitSqlS = "select id, type, asjson from {0} where type = ANY(?) and latest = true order by id limit {1,number,#} offset {2,number,#}";

	protected final DataType dataType;
	protected final Connection connection;
	protected final TransactionResult txResult;
	protected final boolean versioningEnabled;
	protected final List<DaoCommand> commands;

	public PostgresqlDao(DataType dataType, Connection connection, TransactionResult txResult,
			boolean versioningEnabled) {
		this.dataType = dataType;
		this.connection = connection;
		this.txResult = txResult;
		this.versioningEnabled = versioningEnabled;
		this.commands = new ArrayList<>();
	}

	@Override
	public boolean supportsPaging() {
		return true;
	}

	public DataType getDataType() {
		return this.dataType;
	}

	protected abstract String getTableName();

	protected abstract T parseFromXml(String id, String type, SQLXML xml);

	protected abstract T parseFromJson(String id, String type, String json);

	protected SQLXML createSqlXml(T t) throws SQLException, SAXException {
		SQLXML sqlxml = this.connection.createSQLXML();
		SAXResult saxResult = sqlxml.setResult(SAXResult.class);
		ContentHandler contentHandler = saxResult.getHandler();
		contentHandler.startDocument();
		t.accept(new StrolchElementToSaxVisitor(contentHandler));
		contentHandler.endDocument();
		return sqlxml;
	}

	protected SQLXML writeObject(PreparedStatement preparedStatement, T t, int index)
			throws SQLException, SAXException {

		SQLXML sqlxml = null;
		if (this.dataType == DataType.xml) {
			sqlxml = createSqlXml(t);
			preparedStatement.setSQLXML(index, sqlxml);
		} else {
			PGobject jsonObj = new PGobject();
			jsonObj.setType("json");
			jsonObj.setValue(t.accept(new StrolchRootElementToJsonVisitor()).toString());
			preparedStatement.setObject(index, jsonObj);
		}

		return sqlxml;
	}

	protected T parseDbObject(ResultSet result, String id, String type) throws SQLException {
		if (this.dataType == DataType.xml) {

			SQLXML sqlxml = result.getSQLXML("asxml");
			return parseFromXml(id, type, sqlxml);

		} else if (this.dataType == DataType.json) {

			PGobject pGobject = (PGobject) result.getObject("asjson");
			String json = pGobject.getValue();
			return parseFromJson(id, type, json);

		} else {
			throw new IllegalStateException("Unhandled DataType " + this.dataType);
		}
	}

	@Override
	public long querySize() {
		String sql = MessageFormat.format(querySizeSqlS, getTableName());
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
	public long querySize(String... types) {
		if (types.length == 0)
			return querySize();

		String sql = MessageFormat.format(querySizeOfTypeSqlS, getTableName());

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
	public Set<String> queryTypes() {

		String sql = MessageFormat.format(queryTypesSqlS, getTableName());

		Set<String> keySet = new HashSet<>();
		try (PreparedStatement statement = this.connection.prepareStatement(sql)) {

			try (ResultSet result = statement.executeQuery()) {
				while (result.next()) {
					keySet.add(result.getString("type"));
				}
			}

		} catch (SQLException e) {
			throw new StrolchPersistenceException("Failed to query types due to: " + e.getMessage(), e);
		}

		return keySet;
	}

	@Override
	public T queryBy(String type, String id, int versionNr) {

		String sql = getSql(queryByVersionAsXmlSqlS, queryByVersionAsJsonSqlS);

		try (PreparedStatement statement = this.connection.prepareStatement(sql)) {
			statement.setString(1, type);
			statement.setString(2, id);
			statement.setInt(3, versionNr);

			try (ResultSet result = statement.executeQuery()) {
				if (!result.next())
					return null;

				T t = parseDbObject(result, id, type);

				int v = result.getInt("version");
				if (v != versionNr)
					throw new StrolchPersistenceException(
							"Requested version " + versionNr + " != " + v + " for " + t.getLocator());

				if (result.next())
					throw new StrolchPersistenceException("Non unique result for query: " + sql);
				return t;
			}

		} catch (SQLException e) {
			throw new StrolchPersistenceException("Failed to query types due to: " + e.getMessage(), e);
		}
	}

	@Override
	public List<T> queryVersionsFor(String type, String id) {

		String sql = getSql(queryVersionsAsXmlForSqlS, queryVersionsAsJsonForSqlS);

		List<T> list = new ArrayList<>(1);
		try (PreparedStatement statement = this.connection.prepareStatement(sql)) {
			statement.setString(1, type);
			statement.setString(2, id);

			try (ResultSet result = statement.executeQuery()) {

				while (result.next()) {

					T t = parseDbObject(result, id, type);
					list.add(t);
				}

				return list;
			}

		} catch (SQLException e) {
			throw new StrolchPersistenceException("Failed to query types due to: " + e.getMessage(), e);
		}
	}

	@Override
	public int queryLatestVersionFor(String type, String id) {

		String sql = MessageFormat.format(queryLatestVersionNumberForSqlS, getTableName());

		try (PreparedStatement statement = this.connection.prepareStatement(sql)) {
			statement.setString(1, type);
			statement.setString(2, id);

			try (ResultSet result = statement.executeQuery()) {
				result.next();
				int count = result.getInt(1);
				int max = result.getInt(2);
				if (count == 0)
					return -1;
				else
					return max;
			}

		} catch (SQLException e) {
			throw new StrolchPersistenceException("Failed to query types due to: " + e.getMessage(), e);
		}
	}

	@Override
	public long queryVersionsSizeFor(String type, String id) {

		String sql = MessageFormat.format(queryVersionsSizeForSqlS, getTableName());

		try (PreparedStatement statement = this.connection.prepareStatement(sql)) {
			statement.setString(1, type);
			statement.setString(2, id);

			try (ResultSet result = statement.executeQuery()) {
				result.next();
				return result.getLong(1);
			}

		} catch (SQLException e) {
			throw new StrolchPersistenceException("Failed to query types due to: " + e.getMessage(), e);
		}
	}

	@Override
	public List<T> queryAll() {
		return queryAll(Integer.MAX_VALUE, 0);
	}

	@Override
	public List<T> queryAll(long limit, long offset) {

		List<T> list = new ArrayList<>();

		String sql = getLimitSql(limit, offset, queryAllAsXmlSqlS, queryAllAsJsonSqlS, queryAllAsXmlLimitSqlS,
				queryAllAsJsonLimitSqlS);

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
	public List<T> queryAll(String... types) {
		return queryAll(Integer.MAX_VALUE, 0, types);
	}

	@Override
	public List<T> queryAll(long limit, long offset, String... types) {
		if (types.length == 0)
			return queryAll(limit, offset);

		List<T> list = new ArrayList<>();

		String sql = getLimitSql(limit, offset, queryAllByTypeAsXmlSqlS, queryAllByTypeAsJsonSqlS,
				queryAllByTypeAsXmlLimitSqlS, queryAllByTypeAsJsonLimitSqlS);

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

	protected String getLimitSql(long limit, long offset, String xmlSql, String jsonSql, String xmlLimitSql,
			String jsonLimitSql) {

		String sql;
		if (limit == Integer.MAX_VALUE) {
			return getSql(xmlSql, jsonSql);
		}

		if (this.dataType == DataType.xml)
			sql = xmlLimitSql;
		else if (this.dataType == DataType.json)
			sql = jsonLimitSql;
		else
			throw new IllegalStateException("Unhandled DataType " + this.dataType);

		return MessageFormat.format(sql, getTableName(), limit, offset);
	}

	protected String getSql(String xmlSql, String jsonSql) {

		String sql;
		if (this.dataType == DataType.xml)
			sql = xmlSql;
		else if (this.dataType == DataType.json)
			sql = jsonSql;
		else
			throw new IllegalStateException("Unhandled DataType " + this.dataType);

		return MessageFormat.format(sql, getTableName());
	}

	@Override
	public void save(T res) {
		this.commands.add(txResult -> {
			internalSave(res);
			txResult.incCreated(1);
		});
	}

	@Override
	public void saveAll(List<T> elements) {
		this.commands.add(txResult -> {
			for (T element : elements) {
				internalSave(element);
			}
			txResult.incCreated(elements.size());
		});
	}

	@Override
	public void update(T element) {
		this.commands.add(txResult -> {
			internalUpdate(element);
			txResult.incUpdated(1);
		});
	}

	@Override
	public void updateAll(List<T> elements) {
		this.commands.add(txResult -> {
			for (T element : elements) {
				internalUpdate(element);
			}
			txResult.incUpdated(elements.size());
		});
	}

	@Override
	public void remove(T element) {
		this.commands.add(txResult -> {
			internalRemove(element);
			txResult.incDeleted(1);
		});
	}

	@Override
	public void removeAll(List<T> elements) {
		this.commands.add(txResult -> {
			for (T element : elements) {
				internalRemove(element);
			}
			txResult.incDeleted(elements.size());
		});
	}

	@Override
	public long removeAll() {

		final long toRemove = querySize();

		this.commands.add(txResult -> {
			internalRemoveAll();
			txResult.incDeleted(toRemove);
		});

		return toRemove;
	}

	@Override
	public long removeAllBy(String type) {

		long toRemove = querySize(type);

		this.commands.add(txResult -> {
			internalRemoveAllBy(type);
			txResult.incDeleted(toRemove);
		});

		return toRemove;
	}

	@Override
	public void removeVersion(T element) throws StrolchPersistenceException {
		this.commands.add(txResult -> {
			internalRemoveVersion(element);
			txResult.incDeleted(1);
		});
	}

	protected abstract void internalSave(T element);

	protected abstract void internalUpdate(T element);

	protected void internalRemove(T element) {

		// first find out how many there are
		long count;
		String sql = MessageFormat.format(querySizeOfElementSqlS, getTableName());
		try (PreparedStatement statement = this.connection.prepareStatement(sql)) {
			statement.setString(1, element.getType());
			statement.setString(2, element.getId());

			try (ResultSet result = statement.executeQuery()) {
				result.next();
				count = result.getLong(1);
			}

		} catch (SQLException e) {
			throw new StrolchPersistenceException(MessageFormat
					.format("Failed to remove {0} due to {1}", element.getLocator(), e.getLocalizedMessage()), e);
		}

		if (count == 0) {
			throw new StrolchPersistenceException(
					MessageFormat.format("Failed to remove {0} as it does not exist!", element.getLocator()));
		}

		sql = MessageFormat.format(deleteElementSqlS, getTableName());
		try (PreparedStatement preparedStatement = this.connection.prepareStatement(sql)) {
			preparedStatement.setString(1, element.getId());

			int modCount = preparedStatement.executeUpdate();
			if (modCount != count) {
				String msg = "Expected to delete {0} element with id {1} but SQL statement deleted {2} elements!";
				msg = MessageFormat.format(msg, count, element.getId(), modCount);
				throw new StrolchPersistenceException(msg);
			}

		} catch (SQLException e) {
			throw new StrolchPersistenceException(MessageFormat
					.format("Failed to remove {0} due to {1}", element.getLocator(), e.getLocalizedMessage()), e);
		}
	}

	private void internalRemoveVersion(T element) {
		String sql = MessageFormat.format(deleteVersionSqlS, getTableName());
		try (PreparedStatement preparedStatement = this.connection.prepareStatement(sql)) {
			preparedStatement.setString(1, element.getType());
			preparedStatement.setString(2, element.getId());
			preparedStatement.setInt(3, element.getVersion().getVersion());

			int modCount = preparedStatement.executeUpdate();
			if (modCount != 1) {
				String msg = "Expected to delete 1 element with id {0} but SQL statement modified {1} elements! Verify that element {2} is the latest version!";
				msg = MessageFormat.format(msg, element.getId(), modCount, element.getVersion());
				throw new StrolchPersistenceException(msg);
			}

			if (!element.getVersion().isFirstVersion()) {
				sql = MessageFormat.format(updateLatestSqlS, getTableName());
				try (PreparedStatement updateStmt = this.connection.prepareStatement(sql)) {
					int previousVersion = element.getVersion().getPreviousVersion();
					updateStmt.setString(1, element.getType());
					updateStmt.setString(2, element.getId());
					updateStmt.setInt(3, previousVersion);

					modCount = updateStmt.executeUpdate();
					if (modCount != 1) {
						String msg = "Expected to update 1 element with id {0} but SQL statement modified {1} elements! Verify that element {2} with version {3} exists!";
						msg = MessageFormat
								.format(msg, element.getId(), modCount, element.getLocator(), previousVersion);
						throw new StrolchPersistenceException(msg);
					}

				}
			}

		} catch (SQLException e) {
			throw new StrolchPersistenceException(MessageFormat
					.format("Failed to remove version {0} due to {1}", element.getLocator(), e.getLocalizedMessage()),
					e);
		}
	}

	protected void internalRemoveAll() {
		String sql = MessageFormat.format(deleteAllSqlS, getTableName());
		try (PreparedStatement preparedStatement = this.connection.prepareStatement(sql)) {

			preparedStatement.executeUpdate();

		} catch (SQLException e) {
			throw new StrolchPersistenceException(
					MessageFormat.format("Failed to remove all elements due to {0}", e.getLocalizedMessage()), e);
		}
	}

	protected void internalRemoveAllBy(String type) {
		String sql = MessageFormat.format(deleteAllByTypeSqlS, getTableName());
		try (PreparedStatement preparedStatement = this.connection.prepareStatement(sql)) {
			preparedStatement.setString(1, type);

			preparedStatement.executeUpdate();

		} catch (SQLException e) {
			throw new StrolchPersistenceException(MessageFormat
					.format("Failed to remove all elements of type {0} due to {1}", type, e.getLocalizedMessage()), e);
		}
	}

	@Override
	public void flush() {
		// even though we support rollback we can clear the commands here even if we performed them because the DB transaction will be rolled back
		for (DaoCommand command : this.commands) {
			command.doComand(this.txResult);
		}
		this.commands.clear();
	}
}
