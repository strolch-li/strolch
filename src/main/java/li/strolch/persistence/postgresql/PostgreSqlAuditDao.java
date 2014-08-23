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

import java.sql.Date;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import li.strolch.model.audit.AccessType;
import li.strolch.model.audit.Audit;
import li.strolch.model.audit.AuditQuery;
import li.strolch.model.audit.AuditVisitor;
import li.strolch.persistence.api.AuditDao;
import li.strolch.persistence.api.StrolchPersistenceException;
import ch.eitchnet.utils.collections.DateRange;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class PostgreSqlAuditDao implements AuditDao {

	private PostgreSqlStrolchTransaction tx;

	/**
	 * @param postgreSqlStrolchTransaction
	 */
	public PostgreSqlAuditDao(PostgreSqlStrolchTransaction postgreSqlStrolchTransaction) {
		this.tx = postgreSqlStrolchTransaction;
	}

	@Override
	public boolean hasElement(String type, Long id) {
		String sql = "select count(*) from audits where element_type = ? and id = ?";
		try (PreparedStatement statement = this.tx.getConnection().prepareStatement(sql)) {

			statement.setString(1, type);
			statement.setLong(2, id);

			try (ResultSet result = statement.executeQuery()) {
				result.next();
				long numberOfElements = result.getLong(1);
				if (numberOfElements == 0)
					return false;
				if (numberOfElements == 1)
					return true;

				String msg = MessageFormat.format("Non unique number of elements with type {0} and id {1}", type, id);
				throw new StrolchPersistenceException(msg);
			}

		} catch (SQLException e) {
			throw new StrolchPersistenceException("Failed to query size due to: " + e.getMessage(), e);
		}
	}

	@Override
	public long querySize(DateRange dateRange) {
		String sql = "select count(*) from audits where date between ? and ?";
		try (PreparedStatement statement = this.tx.getConnection().prepareStatement(sql)) {

			statement.setDate(1, new Date(dateRange.getFromDate().getTime()), Calendar.getInstance());
			statement.setDate(2, new Date(dateRange.getToDate().getTime()), Calendar.getInstance());

			try (ResultSet result = statement.executeQuery()) {
				result.next();
				return result.getLong(1);
			}

		} catch (SQLException e) {
			throw new StrolchPersistenceException("Failed to query size due to: " + e.getMessage(), e);
		}
	}

	@Override
	public long querySize(String type, DateRange dateRange) {
		String sql = "select count(*) from audits where element_type = ? and date between ? and ?";
		try (PreparedStatement statement = this.tx.getConnection().prepareStatement(sql)) {

			statement.setString(1, type);
			statement.setDate(2, new Date(dateRange.getFromDate().getTime()), Calendar.getInstance());
			statement.setDate(3, new Date(dateRange.getToDate().getTime()), Calendar.getInstance());

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

		String sql = "select distinct element_type from audits";
		try (PreparedStatement statement = this.tx.getConnection().prepareStatement(sql)) {
			try (ResultSet result = statement.executeQuery()) {
				while (result.next()) {
					keySet.add(result.getString("element_type"));
				}
			}
		} catch (SQLException e) {
			throw new StrolchPersistenceException("Failed to query types due to: " + e.getMessage(), e);
		}

		return keySet;
	}

	@Override
	public Audit queryBy(String type, Long id) {

		String sql = "select id, username, firstname, lastname, date, element_type, element_accessed, new_version, action, access_type from audits where element_type = ? and id = ?";
		try (PreparedStatement statement = this.tx.getConnection().prepareStatement(sql)) {

			statement.setString(1, type);
			statement.setLong(2, id);

			try (ResultSet result = statement.executeQuery()) {
				if (!result.next()) {
					return null;
				}
				Audit audit = auditFrom(result);
				if (result.next())
					throw new StrolchPersistenceException("Non unique result for query: " + sql);
				return audit;
			}
		} catch (SQLException e) {
			throw new StrolchPersistenceException("Failed to query types due to: " + e.getMessage(), e);
		}
	}

	@Override
	public List<Audit> queryAll(String type, DateRange dateRange) {
		List<Audit> list = new ArrayList<>();
		String sql = "select id, username, firstname, lastname, date, element_type, element_accessed, new_version, action, access_type from audits where element_type = ? and date between ? and ?";
		try (PreparedStatement statement = this.tx.getConnection().prepareStatement(sql)) {

			statement.setString(1, type);
			statement.setDate(2, new Date(dateRange.getFromDate().getTime()), Calendar.getInstance());
			statement.setDate(3, new Date(dateRange.getToDate().getTime()), Calendar.getInstance());

			try (ResultSet result = statement.executeQuery()) {
				while (result.next()) {
					list.add(auditFrom(result));
				}
			}

		} catch (SQLException e) {
			throw new StrolchPersistenceException("Failed to query types due to: " + e.getMessage(), e);
		}

		return list;
	}

	@Override
	public void save(Audit audit) {
		String sql = "insert into audits (id, username, firstname, lastname, date, element_type, element_accessed, new_version, action, access_type) values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?::access_type)";
		try (PreparedStatement preparedStatement = tx.getConnection().prepareStatement(sql)) {

			setAuditFields(audit, preparedStatement);

			int count = preparedStatement.executeUpdate();
			if (count != 1) {
				throw new StrolchPersistenceException(MessageFormat.format(
						"Expected to create 1 record, but created {0} for audit {2}", count, audit.getId()));
			}

		} catch (SQLException e) {
			throw new StrolchPersistenceException(MessageFormat.format("Failed to update Audit {0} due to {1}", audit,
					e.getLocalizedMessage()), e);
		}
	}

	@Override
	public void saveAll(List<Audit> audits) {
		for (Audit audit : audits) {
			save(audit);
		}
	}

	@Override
	public void update(Audit audit) {
		String sql = "update audits set id = ?, username = ?, firstname = ?, lastname = ?, date = ?, element_type = ?, element_accessed = ?, new_version = ?, action = ?, access_type = ?::access_type where id = ?";
		try (PreparedStatement preparedStatement = tx.getConnection().prepareStatement(sql)) {

			setAuditFields(audit, preparedStatement);
			preparedStatement.setLong(11, audit.getId());

			int count = preparedStatement.executeUpdate();
			if (count != 1) {
				throw new StrolchPersistenceException(MessageFormat.format(
						"Expected to update 1 record, but updated {0} for audit {2}", count, audit.getId()));
			}

		} catch (SQLException e) {
			throw new StrolchPersistenceException(MessageFormat.format("Failed to update Audit {0} due to {1}", audit,
					e.getLocalizedMessage()), e);
		}
	}

	@Override
	public void updateAll(List<Audit> audits) {
		for (Audit audit : audits) {
			update(audit);
		}
	}

	@Override
	public void remove(Audit audit) {
		String sql = "delete from audits where id = ?";
		try (PreparedStatement preparedStatement = this.tx.getConnection().prepareStatement(sql)) {

			preparedStatement.setLong(1, audit.getId());

			int count = preparedStatement.executeUpdate();
			if (count != 1) {
				String msg = "Expected to delete 1 audit with id {0} but deleted {1} elements!";
				msg = MessageFormat.format(msg, audit.getId(), count);
				throw new StrolchPersistenceException(msg);
			}

		} catch (SQLException e) {
			throw new StrolchPersistenceException(MessageFormat.format("Failed to remove {0} due to {2}",
					audit.getId(), e.getLocalizedMessage()), e);
		}
	}

	@Override
	public void removeAll(List<Audit> audits) {
		for (Audit audit : audits) {
			remove(audit);
		}
	}

	@Override
	public long removeAll(String type, DateRange dateRange) {
		String sql = "delete from audits where element_type = ? and date between ? and ?";
		try (PreparedStatement preparedStatement = this.tx.getConnection().prepareStatement(sql)) {

			preparedStatement.setString(1, type);
			preparedStatement.setDate(2, new Date(dateRange.getFromDate().getTime()), Calendar.getInstance());
			preparedStatement.setDate(3, new Date(dateRange.getToDate().getTime()), Calendar.getInstance());

			int modCount = preparedStatement.executeUpdate();
			return modCount;

		} catch (SQLException e) {
			throw new StrolchPersistenceException(MessageFormat.format("Failed to remove all elements due to {0}",
					e.getLocalizedMessage()), e);
		}
	}

	@Override
	public <U> List<U> doQuery(AuditQuery query, AuditVisitor<U> auditVisitor) {
		// TODO Auto-generated method stub
		return null;
	}

	private void setAuditFields(Audit audit, PreparedStatement ps) throws SQLException {

		// 1  id = ?, 
		// 2  username = ?, 
		// 3  firstname = ?,
		// 4  lastname = ?, 
		// 5  date = ?, 
		// 6  element_type = ?, 
		// 7  element_accessed = ?, 
		// 8  new_version = ?, 
		// 9  action = ?, 
		// 10 access_type = ?::access_type

		ps.setLong(1, audit.getId());
		ps.setString(2, audit.getUsername());
		ps.setString(3, audit.getFirstname());
		ps.setString(4, audit.getLastname());
		ps.setDate(5, new Date(audit.getDate().getTime()), Calendar.getInstance());
		ps.setString(6, audit.getElementType());
		ps.setString(7, audit.getElementAccessed());

		if (audit.getNewVersion() == null)
			ps.setDate(8, null);
		else
			ps.setDate(8, new Date(audit.getNewVersion().getTime()), Calendar.getInstance());

		ps.setString(9, audit.getAction());
		ps.setString(10, audit.getAccessType().name());
	}

	private Audit auditFrom(ResultSet resultSet) throws SQLException {

		Audit audit = new Audit();
		audit.setId(resultSet.getLong(1));
		audit.setUsername(resultSet.getString(2));
		audit.setFirstname(resultSet.getString(3));
		audit.setLastname(resultSet.getString(4));
		audit.setDate(resultSet.getDate(5));
		audit.setElementType(resultSet.getString(6));
		audit.setElementAccessed(resultSet.getString(7));
		audit.setNewVersion(resultSet.getDate(8));
		audit.setAction(resultSet.getString(9));
		audit.setAccessType(AccessType.valueOf(resultSet.getString(10)));
		return audit;
	}
}
