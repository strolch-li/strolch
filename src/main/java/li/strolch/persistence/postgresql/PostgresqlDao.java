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

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.SQLXML;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import li.strolch.model.StrolchElement;
import li.strolch.persistence.api.ModificationResult;
import li.strolch.persistence.api.StrolchDao;
import li.strolch.persistence.api.StrolchPersistenceException;
import li.strolch.persistence.api.TransactionResult;

@SuppressWarnings("nls")
public abstract class PostgresqlDao<T extends StrolchElement> implements StrolchDao<T> {

	protected PostgreSqlStrolchTransaction tx;
	protected List<DaoCommand> commands;

	/**
	 * @param tx
	 */
	public PostgresqlDao(PostgreSqlStrolchTransaction tx) {
		this.tx = tx;
		this.commands = new ArrayList<>();
	}

	protected abstract String getClassName();

	protected abstract String getTableName();

	protected abstract T parseFromXml(String id, String type, SQLXML xml);

	@Override
	public long querySize() {
		String sql = "select count(*) from " + getTableName();
		try (PreparedStatement statement = this.tx.getConnection().prepareStatement(sql)) {
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

		String sql = "select count(*) from " + getTableName() + " where type = ?";
		try (PreparedStatement statement = this.tx.getConnection().prepareStatement(sql)) {
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
	public Set<String> queryKeySet() {

		Set<String> keySet = new HashSet<>();

		String sql = "select id from " + getTableName();
		try (PreparedStatement statement = this.tx.getConnection().prepareStatement(sql)) {
			try (ResultSet result = statement.executeQuery()) {
				while (result.next()) {
					keySet.add(result.getString("id"));
				}
			}
		} catch (SQLException e) {
			throw new StrolchPersistenceException("Failed to query key set due to: " + e.getMessage(), e);
		}

		return keySet;
	}

	@Override
	public Set<String> queryKeySet(String type) {
		Set<String> keySet = new HashSet<>();

		String sql = "select id from " + getTableName() + " where type = ?";
		try (PreparedStatement statement = this.tx.getConnection().prepareStatement(sql)) {
			statement.setString(1, type);
			try (ResultSet result = statement.executeQuery()) {
				while (result.next()) {
					keySet.add(result.getString("id"));
				}
			}
		} catch (SQLException e) {
			throw new StrolchPersistenceException("Failed to query key set due to: " + e.getMessage(), e);
		}

		return keySet;
	}

	@Override
	public Set<String> queryTypes() {
		Set<String> keySet = new HashSet<>();

		String sql = "select distinct type from " + getTableName();
		try (PreparedStatement statement = this.tx.getConnection().prepareStatement(sql)) {
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
	public T queryBy(String type, String id) {

		String sql = "select id, name, type, asxml from " + getTableName() + " where id = ? and type = ?";
		try (PreparedStatement statement = this.tx.getConnection().prepareStatement(sql)) {
			statement.setString(1, id);
			statement.setString(2, type);
			try (ResultSet result = statement.executeQuery()) {
				if (!result.next()) {
					return null;
				}

				SQLXML sqlxml = result.getSQLXML("asxml");
				T t = parseFromXml(id, type, sqlxml);
				if (result.next())
					throw new StrolchPersistenceException("Non unique result for query: " + sql);
				return t;
			}
		} catch (SQLException e) {
			throw new StrolchPersistenceException("Failed to query types due to: " + e.getMessage(), e);
		}
	}

	@Override
	public List<T> queryAll() {

		List<T> list = new ArrayList<>();
		String sql = "select id, name, type, asxml from " + getTableName();
		try (PreparedStatement statement = this.tx.getConnection().prepareStatement(sql)) {
			try (ResultSet result = statement.executeQuery()) {
				while (result.next()) {
					String id = result.getString("id");
					String type = result.getString("type");
					SQLXML sqlxml = result.getSQLXML("asxml");
					T t = parseFromXml(id, type, sqlxml);
					list.add(t);
				}
			}
		} catch (SQLException e) {
			throw new StrolchPersistenceException("Failed to query types due to: " + e.getMessage(), e);
		}

		return list;
	}

	@Override
	public List<T> queryAll(String type) {

		List<T> list = new ArrayList<>();
		String sql = "select id, name, type, asxml from " + getTableName() + " where type = ?";
		try (PreparedStatement statement = this.tx.getConnection().prepareStatement(sql)) {
			statement.setString(1, type);
			try (ResultSet result = statement.executeQuery()) {
				while (result.next()) {
					String id = result.getString("id");
					SQLXML sqlxml = result.getSQLXML("asxml");
					T t = parseFromXml(id, type, sqlxml);
					list.add(t);
				}
			}
		} catch (SQLException e) {
			throw new StrolchPersistenceException("Failed to query types due to: " + e.getMessage(), e);
		}

		return list;
	}

	@Override
	public void save(final T res) {
		this.commands.add(new DaoCommand() {
			@Override
			public void doComand(ModificationResult modificationResult) {
				internalSave(res);
				modificationResult.getCreated().add(res);
			}
		});
	}

	@Override
	public void saveAll(final List<T> elements) {
		this.commands.add(new DaoCommand() {
			@Override
			public void doComand(ModificationResult modificationResult) {
				for (T element : elements) {
					internalSave(element);
				}
				modificationResult.getCreated().addAll(elements);
			}
		});
	}

	@Override
	public void update(final T element) {
		this.commands.add(new DaoCommand() {
			@Override
			public void doComand(ModificationResult modificationResult) {
				internalUpdate(element);
				modificationResult.getUpdated().add(element);
			}
		});
	}

	@Override
	public void updateAll(final List<T> elements) {
		this.commands.add(new DaoCommand() {
			@Override
			public void doComand(ModificationResult modificationResult) {
				for (T element : elements) {
					internalUpdate(element);
				}
				modificationResult.getUpdated().addAll(elements);
			}
		});
	}

	@Override
	public void remove(final T element) {
		this.commands.add(new DaoCommand() {
			@Override
			public void doComand(ModificationResult modificationResult) {
				internalRemove(element);
				modificationResult.getDeleted().add(element);
			}
		});
	}

	@Override
	public void removeAll(final List<T> elements) {
		this.commands.add(new DaoCommand() {
			@Override
			public void doComand(ModificationResult modificationResult) {
				for (T element : elements) {
					internalRemove(element);
				}
				modificationResult.getDeleted().addAll(elements);
			}
		});
	}

	/**
	 * @param element
	 */
	protected abstract void internalSave(T element);

	/**
	 * @param element
	 */
	protected abstract void internalUpdate(T element);

	protected void internalRemove(final T element) {
		String sql = "delete from " + getTableName() + " where id = ?";
		try (PreparedStatement preparedStatement = this.tx.getConnection().prepareStatement(sql)) {

			preparedStatement.setString(1, element.getId());
			preparedStatement.execute();

		} catch (SQLException e) {
			throw new StrolchPersistenceException(MessageFormat.format("Failed to update Order {0} due to {2}",
					element.getLocator(), e.getLocalizedMessage()), e);
		}
	}

	void commit(TransactionResult txResult) {
		ModificationResult modificationResult = new ModificationResult(getClassName());
		txResult.addModificationResult(modificationResult);
		for (DaoCommand command : this.commands) {
			command.doComand(modificationResult);
		}
	}

	void rollback() {
		this.commands.clear();
	}
}
