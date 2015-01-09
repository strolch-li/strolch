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
import java.sql.Timestamp;
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
import ch.eitchnet.utils.helper.StringHelper;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class PostgreSqlAuditDao implements AuditDao {

	public static final String ID = "id";
	public static final String ACCESS_TYPE = "access_type";
	public static final String ACCESS_TYPE_TYPE = "::access_type";
	public static final String ACTION = "action";
	public static final String NEW_VERSION = "new_version";
	public static final String ELEMENT_ACCESSED = "element_accessed";
	public static final String ELEMENT_TYPE = "element_type";
	public static final String DATE = "date";
	public static final String LASTNAME = "lastname";
	public static final String FIRSTNAME = "firstname";
	public static final String USERNAME = "username";
	public static final String FIELDS = StringHelper.commaSeparated(ID, USERNAME, FIRSTNAME, LASTNAME, DATE,
			ELEMENT_TYPE, ELEMENT_ACCESSED, NEW_VERSION, ACTION, ACCESS_TYPE);
	public static final String TABLE_NAME = "audits";

	private PostgreSqlStrolchTransaction tx;

	/**
	 * @param postgreSqlStrolchTransaction
	 */
	public PostgreSqlAuditDao(PostgreSqlStrolchTransaction postgreSqlStrolchTransaction) {
		this.tx = postgreSqlStrolchTransaction;
	}

	@Override
	public boolean hasElement(String type, Long id) {
		String sql = "select count(*) from " + TABLE_NAME + " where " + ELEMENT_TYPE + " = ? and " + ID + " = ?"; //$NON-NLS-1$
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

				String msg = MessageFormat.format("Non unique number of elements with type {0} and id {1}", type, id); //$NON-NLS-1$
				throw new StrolchPersistenceException(msg);
			}

		} catch (SQLException e) {
			throw new StrolchPersistenceException("Failed to query size due to: " + e.getMessage(), e); //$NON-NLS-1$
		}
	}

	@Override
	public long querySize(DateRange dateRange) {
		String sql = "select count(*) from " + TABLE_NAME + " where " + DATE + " between ? and ?"; //$NON-NLS-1$
		try (PreparedStatement statement = this.tx.getConnection().prepareStatement(sql)) {

			statement.setTimestamp(1, new Timestamp(dateRange.getFromDate().getTime()), Calendar.getInstance());
			statement.setTimestamp(2, new Timestamp(dateRange.getToDate().getTime()), Calendar.getInstance());

			try (ResultSet result = statement.executeQuery()) {
				result.next();
				return result.getLong(1);
			}

		} catch (SQLException e) {
			throw new StrolchPersistenceException("Failed to query size due to: " + e.getMessage(), e); //$NON-NLS-1$
		}
	}

	@Override
	public long querySize(String type, DateRange dateRange) {
		String sql = "select count(*) from " + TABLE_NAME + " where " + ELEMENT_TYPE + " = ? and " + DATE + " between ? and ?"; //$NON-NLS-1$
		try (PreparedStatement statement = this.tx.getConnection().prepareStatement(sql)) {

			statement.setString(1, type);
			statement.setTimestamp(2, new Timestamp(dateRange.getFromDate().getTime()), Calendar.getInstance());
			statement.setTimestamp(3, new Timestamp(dateRange.getToDate().getTime()), Calendar.getInstance());

			try (ResultSet result = statement.executeQuery()) {
				result.next();
				return result.getLong(1);
			}

		} catch (SQLException e) {
			throw new StrolchPersistenceException("Failed to query size due to: " + e.getMessage(), e); //$NON-NLS-1$
		}
	}

	@Override
	public Set<String> queryTypes() {
		Set<String> keySet = new HashSet<>();

		String sql = "select distinct " + ELEMENT_TYPE + " from " + TABLE_NAME; //$NON-NLS-1$
		try (PreparedStatement statement = this.tx.getConnection().prepareStatement(sql)) {
			try (ResultSet result = statement.executeQuery()) {
				while (result.next()) {
					keySet.add(result.getString(ELEMENT_TYPE));
				}
			}
		} catch (SQLException e) {
			throw new StrolchPersistenceException("Failed to query types due to: " + e.getMessage(), e); //$NON-NLS-1$
		}

		return keySet;
	}

	@Override
	public Audit queryBy(String type, Long id) {

		String sql = "select " + FIELDS + " from " + TABLE_NAME + " where " + ELEMENT_TYPE + " = ? and " + ID + " = ?"; //$NON-NLS-1$
		try (PreparedStatement statement = this.tx.getConnection().prepareStatement(sql)) {

			statement.setString(1, type);
			statement.setLong(2, id);

			try (ResultSet result = statement.executeQuery()) {
				if (!result.next()) {
					return null;
				}
				Audit audit = auditFrom(result);
				if (result.next())
					throw new StrolchPersistenceException("Non unique result for query: " + sql); //$NON-NLS-1$
				return audit;
			}
		} catch (SQLException e) {
			throw new StrolchPersistenceException("Failed to query types due to: " + e.getMessage(), e); //$NON-NLS-1$
		}
	}

	@Override
	public List<Audit> queryAll(String type, DateRange dateRange) {
		List<Audit> list = new ArrayList<>();
		String sql = "select " + FIELDS + " from " + TABLE_NAME + " where " + ELEMENT_TYPE + " = ? and " + DATE + " between ? and ?"; //$NON-NLS-1$
		try (PreparedStatement statement = this.tx.getConnection().prepareStatement(sql)) {

			statement.setString(1, type);
			statement.setTimestamp(2, new Timestamp(dateRange.getFromDate().getTime()), Calendar.getInstance());
			statement.setTimestamp(3, new Timestamp(dateRange.getToDate().getTime()), Calendar.getInstance());

			try (ResultSet result = statement.executeQuery()) {
				while (result.next()) {
					list.add(auditFrom(result));
				}
			}

		} catch (SQLException e) {
			throw new StrolchPersistenceException("Failed to query types due to: " + e.getMessage(), e); //$NON-NLS-1$
		}

		return list;
	}

	@Override
	public void save(Audit audit) {
		String sql = "insert into " + TABLE_NAME + " (" + FIELDS + ") values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?::access_type)"; //$NON-NLS-1$
		try (PreparedStatement preparedStatement = this.tx.getConnection().prepareStatement(sql)) {

			setAuditFields(audit, preparedStatement);

			int count = preparedStatement.executeUpdate();
			if (count != 1) {
				throw new StrolchPersistenceException(MessageFormat.format(
						"Expected to create 1 record, but created {0} for audit {2}", count, audit.getId())); //$NON-NLS-1$
			}

		} catch (SQLException e) {
			throw new StrolchPersistenceException(MessageFormat.format("Failed to update Audit {0} due to {1}", audit, //$NON-NLS-1$
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
		String sql = "update " + TABLE_NAME + " set " + ID + " = ?, " + USERNAME + " = ?, " + FIRSTNAME + " = ?, "
				+ LASTNAME + " = ?, " + DATE + " = ?, " + ELEMENT_TYPE + " = ?, " + ELEMENT_ACCESSED + " = ?, "
				+ NEW_VERSION + " = ?, " + ACTION + " = ?, " + ACCESS_TYPE + " = ?::access_type where " + ID + " = ?"; //$NON-NLS-1$
		try (PreparedStatement preparedStatement = this.tx.getConnection().prepareStatement(sql)) {

			setAuditFields(audit, preparedStatement);
			preparedStatement.setLong(11, audit.getId());

			int count = preparedStatement.executeUpdate();
			if (count != 1) {
				throw new StrolchPersistenceException(MessageFormat.format(
						"Expected to update 1 record, but updated {0} for audit {2}", count, audit.getId())); //$NON-NLS-1$
			}

		} catch (SQLException e) {
			throw new StrolchPersistenceException(MessageFormat.format("Failed to update Audit {0} due to {1}", audit, //$NON-NLS-1$
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
		String sql = "delete from " + TABLE_NAME + " where " + ID + " = ?"; //$NON-NLS-1$
		try (PreparedStatement preparedStatement = this.tx.getConnection().prepareStatement(sql)) {

			preparedStatement.setLong(1, audit.getId());

			int count = preparedStatement.executeUpdate();
			if (count != 1) {
				String msg = "Expected to delete 1 audit with id {0} but deleted {1} elements!"; //$NON-NLS-1$
				msg = MessageFormat.format(msg, audit.getId(), count);
				throw new StrolchPersistenceException(msg);
			}

		} catch (SQLException e) {
			throw new StrolchPersistenceException(MessageFormat.format("Failed to remove {0} due to {2}", //$NON-NLS-1$
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
		String sql = "delete from " + TABLE_NAME + " where " + ELEMENT_TYPE + " = ? and " + DATE + " between ? and ?"; //$NON-NLS-1$
		try (PreparedStatement preparedStatement = this.tx.getConnection().prepareStatement(sql)) {

			preparedStatement.setString(1, type);
			preparedStatement.setTimestamp(2, new Timestamp(dateRange.getFromDate().getTime()), Calendar.getInstance());
			preparedStatement.setTimestamp(3, new Timestamp(dateRange.getToDate().getTime()), Calendar.getInstance());

			int modCount = preparedStatement.executeUpdate();
			return modCount;

		} catch (SQLException e) {
			throw new StrolchPersistenceException(MessageFormat.format("Failed to remove all elements due to {0}", //$NON-NLS-1$
					e.getLocalizedMessage()), e);
		}
	}

	@Override
	public <U> List<U> doQuery(AuditQuery query, AuditVisitor<U> auditVisitor) {

		PostgreSqlAuditQueryVisitor queryVisitor = new PostgreSqlAuditQueryVisitor(FIELDS);
		query.accept(queryVisitor);
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
		ps.setTimestamp(5, new Timestamp(audit.getDate().getTime()), Calendar.getInstance());
		ps.setString(6, audit.getElementType());
		ps.setString(7, audit.getElementAccessed());

		if (audit.getNewVersion() == null)
			ps.setDate(8, null);
		else
			ps.setTimestamp(8, new Timestamp(audit.getNewVersion().getTime()), Calendar.getInstance());

		ps.setString(9, audit.getAction());
		ps.setString(10, audit.getAccessType().name());
	}

	private Audit auditFrom(ResultSet resultSet) throws SQLException {

		Audit audit = new Audit();
		audit.setId(resultSet.getLong(1));
		audit.setUsername(resultSet.getString(2));
		audit.setFirstname(resultSet.getString(3));
		audit.setLastname(resultSet.getString(4));
		audit.setDate(resultSet.getTimestamp(5));
		audit.setElementType(resultSet.getString(6));
		audit.setElementAccessed(resultSet.getString(7));
		audit.setNewVersion(resultSet.getTimestamp(8));
		audit.setAction(resultSet.getString(9));
		audit.setAccessType(AccessType.valueOf(resultSet.getString(10)));
		return audit;
	}
}
