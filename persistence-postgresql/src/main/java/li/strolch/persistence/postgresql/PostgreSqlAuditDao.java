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

import static li.strolch.utils.helper.StringHelper.commaSeparated;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.text.MessageFormat;
import java.util.*;

import li.strolch.model.audit.AccessType;
import li.strolch.model.audit.Audit;
import li.strolch.persistence.api.AuditDao;
import li.strolch.persistence.api.StrolchPersistenceException;
import li.strolch.utils.collections.DateRange;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class PostgreSqlAuditDao implements AuditDao {

	public static final String ID = "id";
	public static final String ACCESS_TYPE = "access_type";
	public static final String ACCESS_TYPE_TYPE = "::access_type";
	public static final String ACTION = "action";
	public static final String NEW_VERSION = "new_version";
	public static final String ELEMENT_TYPE = "element_type";
	public static final String ELEMENT_SUB_TYPE = "element_sub_type";
	public static final String ELEMENT_ACCESSED = "element_accessed";
	public static final String DATE = "date";
	public static final String LASTNAME = "lastname";
	public static final String FIRSTNAME = "firstname";
	public static final String USERNAME = "username";
	public static final String FIELDS = commaSeparated(ID, USERNAME, FIRSTNAME, LASTNAME, DATE, ELEMENT_TYPE,
			ELEMENT_SUB_TYPE, ELEMENT_ACCESSED, NEW_VERSION, ACTION, ACCESS_TYPE);
	public static final String TABLE_NAME = "audits";

	private static final String hasElementSql = "select count(*) from audits where element_type = ? and id = ?";
	private static final String querySizeSql = "select count(*) from audits where date between ? and ?";
	private static final String querySizeTypeSql = "select count(*) from audits where element_type = ? and date between ? and ?";
	private static final String queryTypesSql = "select distinct element_type from audits";
	private static final String queryBySql = "select " + FIELDS + " from audits where element_type = ? and ID = ?";
	private static final String queryAllSql =
			"select " + FIELDS + " from audits where element_type = ? and date between ? and ?";
	private static final String insertSql =
			"insert into audits (" + FIELDS + ") values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?::access_type)";
	private static final String updateSql = "update audits set id = ?, username = ?, firstname = ?, lastname = ?, date = ?, element_type = ?, element_sub_type = ?, element_accessed = ?, new_version = ?, action = ?, access_type = ?::access_type where id = ?";
	private static final String removeSql = "delete from audits where id = ?";
	private static final String removeAllSql = "delete from audits where element_type = ? and date between ? and ?";

	private final PostgreSqlStrolchTransaction tx;

	public PostgreSqlAuditDao(PostgreSqlStrolchTransaction postgreSqlStrolchTransaction) {
		this.tx = postgreSqlStrolchTransaction;
	}

	@Override
	public boolean hasElement(String type, Long id) {

		try (PreparedStatement statement = this.tx.getConnection().prepareStatement(hasElementSql)) {

			statement.setString(1, type);
			statement.setLong(2, id);

			try (ResultSet result = statement.executeQuery()) {
				result.next();
				long numberOfElements = result.getLong(1);
				if (numberOfElements == 0)
					return false;
				if (numberOfElements == 1)
					return true;

				String msg = MessageFormat
						.format("Non unique number of elements with type {0} and id {1}", type, id); //$NON-NLS-1$
				throw new StrolchPersistenceException(msg);
			}

		} catch (SQLException e) {
			throw new StrolchPersistenceException("Failed to query size due to: " + e.getMessage(), e); //$NON-NLS-1$
		}
	}

	@Override
	public long querySize(DateRange dateRange) {
		try (PreparedStatement statement = this.tx.getConnection().prepareStatement(querySizeSql)) {

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
		try (PreparedStatement statement = this.tx.getConnection().prepareStatement(querySizeTypeSql)) {

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

		try (PreparedStatement statement = this.tx.getConnection().prepareStatement(queryTypesSql)) {
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

		try (PreparedStatement statement = this.tx.getConnection().prepareStatement(queryBySql)) {

			statement.setString(1, type);
			statement.setLong(2, id);

			try (ResultSet result = statement.executeQuery()) {
				if (!result.next()) {
					return null;
				}
				Audit audit = auditFrom(result);
				if (result.next())
					throw new StrolchPersistenceException(
							"Non unique result for query: " + queryBySql + " (type=" + type + ", id="
									+ id); //$NON-NLS-1$
				return audit;
			}
		} catch (SQLException e) {
			throw new StrolchPersistenceException("Failed to query types due to: " + e.getMessage(), e); //$NON-NLS-1$
		}
	}

	@Override
	public List<Audit> queryAll(String type, DateRange dateRange) {
		List<Audit> list = new ArrayList<>();
		try (PreparedStatement statement = this.tx.getConnection().prepareStatement(queryAllSql)) {

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
		try (PreparedStatement preparedStatement = this.tx.getConnection().prepareStatement(insertSql)) {

			setAuditFields(audit, preparedStatement);

			int count = preparedStatement.executeUpdate();
			if (count != 1) {
				throw new StrolchPersistenceException(MessageFormat
						.format("Expected to insert 1 record, but inserted {0} for audit {2}", count,
								audit.getId())); //$NON-NLS-1$
			}

		} catch (SQLException e) {
			throw new StrolchPersistenceException(
					MessageFormat.format("Failed to insert Audit {0} due to {1}", audit, //$NON-NLS-1$
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
		try (PreparedStatement preparedStatement = this.tx.getConnection().prepareStatement(updateSql)) {

			setAuditFields(audit, preparedStatement);
			preparedStatement.setLong(12, audit.getId());

			int count = preparedStatement.executeUpdate();
			if (count != 1) {
				throw new StrolchPersistenceException(MessageFormat
						.format("Expected to update 1 record, but updated {0} for audit {2}", count,
								audit.getId())); //$NON-NLS-1$
			}

		} catch (SQLException e) {
			throw new StrolchPersistenceException(
					MessageFormat.format("Failed to update Audit {0} due to {1}", audit, //$NON-NLS-1$
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
		try (PreparedStatement preparedStatement = this.tx.getConnection().prepareStatement(removeSql)) {

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
		try (PreparedStatement preparedStatement = this.tx.getConnection().prepareStatement(removeAllSql)) {

			preparedStatement.setString(1, type);
			preparedStatement.setTimestamp(2, new Timestamp(dateRange.getFromDate().getTime()), Calendar.getInstance());
			preparedStatement.setTimestamp(3, new Timestamp(dateRange.getToDate().getTime()), Calendar.getInstance());

			return preparedStatement.executeUpdate();

		} catch (SQLException e) {
			throw new StrolchPersistenceException(
					MessageFormat.format("Failed to remove all elements due to {0}", //$NON-NLS-1$
							e.getLocalizedMessage()), e);
		}
	}

	private void setAuditFields(Audit audit, PreparedStatement ps) throws SQLException {

		// 1  id = ?, 
		// 2  username = ?, 
		// 3  firstname = ?,
		// 4  lastname = ?, 
		// 5  date = ?, 
		// 6  element_type = ?, 
		// 7  element_sub_type = ?, 
		// 8  element_accessed = ?, 
		// 9  new_version = ?, 
		// 10 action = ?, 
		// 11 access_type = ?::access_type

		ps.setLong(1, audit.getId());
		ps.setString(2, audit.getUsername());
		ps.setString(3, audit.getFirstname());
		ps.setString(4, audit.getLastname());
		ps.setTimestamp(5, new Timestamp(audit.getDate().getTime()), Calendar.getInstance());
		ps.setString(6, audit.getElementType());
		ps.setString(7, audit.getElementSubType());
		ps.setString(8, audit.getElementAccessed());

		if (audit.getNewVersion() == null)
			ps.setDate(9, null);
		else
			ps.setTimestamp(9, new Timestamp(audit.getNewVersion().getTime()), Calendar.getInstance());

		ps.setString(10, audit.getAction());
		ps.setString(11, audit.getAccessType().name());
	}

	private Audit auditFrom(ResultSet resultSet) throws SQLException {

		Audit audit = new Audit();
		audit.setId(resultSet.getLong(1));
		audit.setUsername(resultSet.getString(2));
		audit.setFirstname(resultSet.getString(3));
		audit.setLastname(resultSet.getString(4));
		audit.setDate(resultSet.getTimestamp(5));
		audit.setElementType(resultSet.getString(6));
		audit.setElementSubType(resultSet.getString(7));
		audit.setElementAccessed(resultSet.getString(8));
		audit.setNewVersion(resultSet.getTimestamp(9));
		audit.setAction(resultSet.getString(10));
		audit.setAccessType(AccessType.valueOf(resultSet.getString(11)));
		return audit;
	}
}
