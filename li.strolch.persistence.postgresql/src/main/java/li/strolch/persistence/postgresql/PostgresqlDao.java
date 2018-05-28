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

import java.sql.*;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import li.strolch.model.StrolchRootElement;
import li.strolch.model.Version;
import li.strolch.persistence.api.StrolchDao;
import li.strolch.persistence.api.StrolchPersistenceException;
import li.strolch.persistence.api.TransactionResult;

@SuppressWarnings("nls")
public abstract class PostgresqlDao<T extends StrolchRootElement> implements StrolchDao<T> {

	protected Connection connection;
	protected final TransactionResult txResult;
	protected final boolean versioningEnabled;
	protected List<DaoCommand> commands;

	public PostgresqlDao(Connection connection, TransactionResult txResult, boolean versioningEnabled) {
		this.connection = connection;
		this.txResult = txResult;
		this.versioningEnabled = versioningEnabled;
		this.commands = new ArrayList<>();
	}

	protected abstract String getClassName();

	protected abstract String getTableName();

	protected abstract T parseFromXml(String id, String type, SQLXML xml);

	@Override
	public long querySize() {
		String sql = "select count(*) from " + getTableName() + " where latest = true";
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
	public long querySize(String type) {
		String sql = "select count(*) from " + getTableName() + " where type = ? and latest = true";
		try (PreparedStatement statement = this.connection.prepareStatement(sql)) {
			statement.setString(1, type);

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
		Set<String> keySet = new HashSet<>();

		String sql = "select distinct type from " + getTableName() + " where latest = true";
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

		String sql = "select id, name, type, version, created_by, created_at, deleted, asxml from " + getTableName()
				+ " where type = ? and id = ? and version = ?";
		try (PreparedStatement statement = this.connection.prepareStatement(sql)) {
			statement.setString(1, type);
			statement.setString(2, id);
			statement.setInt(3, versionNr);

			try (ResultSet result = statement.executeQuery()) {
				if (!result.next()) {
					return null;
				}

				SQLXML sqlxml = result.getSQLXML("asxml");
				T t = parseFromXml(id, type, sqlxml);

				int v = result.getInt(4);
				if (v != versionNr)
					throw new StrolchPersistenceException(
							"Requested version " + versionNr + " != " + v + " for " + t.getLocator());
				String createdBy = result.getString(5);
				Date createdAt = new Date(result.getDate(6).getTime());
				boolean deleted = result.getBoolean(7);
				Version version = new Version(t.getLocator(), v, createdBy, createdAt, deleted);
				t.setVersion(version);

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

		String sql = "select id, name, type, version, created_by, created_at, deleted, asxml from " + getTableName()
				+ " where type = ? and id = ? order by version";

		List<T> list = new ArrayList<>(1);

		try (PreparedStatement statement = this.connection.prepareStatement(sql)) {
			statement.setString(1, type);
			statement.setString(2, id);

			try (ResultSet result = statement.executeQuery()) {

				while (result.next()) {
					SQLXML sqlxml = result.getSQLXML("asxml");
					T t = parseFromXml(id, type, sqlxml);

					int v = result.getInt(4);
					String createdBy = result.getString(5);
					Date createdAt = new Date(result.getDate(6).getTime());
					boolean deleted = result.getBoolean(7);
					Version version = new Version(t.getLocator(), v, createdBy, createdAt, deleted);
					t.setVersion(version);

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

		String sql = "select count(*), max(version) from " + getTableName() + " where type = ? and id = ?";

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

		String sql = "select count(*) from " + getTableName() + " where type = ? and id = ?";

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

		List<T> list = new ArrayList<>();

		String sql = "select id, name, type, version, created_by, created_at, deleted, asxml from " + getTableName()
				+ " where latest = true";
		try (PreparedStatement statement = this.connection.prepareStatement(sql)) {

			try (ResultSet result = statement.executeQuery()) {
				while (result.next()) {
					String id = result.getString("id");
					String type = result.getString("type");
					SQLXML sqlxml = result.getSQLXML("asxml");
					T t = parseFromXml(id, type, sqlxml);

					int v = result.getInt(4);
					String createdBy = result.getString(5);
					Date createdAt = new Date(result.getDate(6).getTime());
					boolean deleted = result.getBoolean(7);
					Version version = new Version(t.getLocator(), v, createdBy, createdAt, deleted);
					t.setVersion(version);

					list.add(t);
				}

				return list;
			}

		} catch (SQLException e) {
			throw new StrolchPersistenceException("Failed to query types due to: " + e.getMessage(), e);
		}
	}

	@Override
	public List<T> queryAll(String type) {

		List<T> list = new ArrayList<>();

		String sql = "select id, name, type, version, created_by, created_at, deleted, asxml from " + getTableName()
				+ " where type = ? and latest = true";
		try (PreparedStatement statement = this.connection.prepareStatement(sql)) {
			statement.setString(1, type);

			try (ResultSet result = statement.executeQuery()) {
				while (result.next()) {
					String id = result.getString("id");
					SQLXML sqlxml = result.getSQLXML("asxml");
					T t = parseFromXml(id, type, sqlxml);

					int v = result.getInt(4);
					String createdBy = result.getString(5);
					Date createdAt = new Date(result.getDate(6).getTime());
					boolean deleted = result.getBoolean(7);
					Version version = new Version(t.getLocator(), v, createdBy, createdAt, deleted);
					t.setVersion(version);

					list.add(t);
				}

				return list;
			}

		} catch (SQLException e) {
			throw new StrolchPersistenceException("Failed to query types due to: " + e.getMessage(), e);
		}
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
		long count = 0;
		String sql = "select count(*) from " + getTableName() + " where type = ? and id = ?";
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

		sql = "delete from " + getTableName() + " where id = ?";
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
		String sql = "delete from " + getTableName() + " where type = ? and id = ? and version = ? and latest = true";
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
				sql = "update " + getTableName() + " set latest = true where type = ? and id = ? and version = ?";
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
		String sql = "delete from " + getTableName();
		try (PreparedStatement preparedStatement = this.connection.prepareStatement(sql)) {

			preparedStatement.executeUpdate();

		} catch (SQLException e) {
			throw new StrolchPersistenceException(
					MessageFormat.format("Failed to remove all elements due to {0}", e.getLocalizedMessage()), e);
		}
	}

	protected void internalRemoveAllBy(String type) {
		String sql = "delete from " + getTableName() + " where type = ?";
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
