/*
 * Copyright (c) 2013-2025 Robert von Burg <eitch@eitchnet.ch>
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

import li.strolch.model.audit.AccessType;
import li.strolch.model.audit.Audit;
import li.strolch.persistence.api.AuditDao;
import li.strolch.persistence.api.StrolchPersistenceException;
import li.strolch.utils.collections.DateRange;
import org.postgresql.util.PGobject;

import java.sql.*;
import java.text.MessageFormat;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;

import static li.strolch.utils.helper.StringHelper.commaSeparated;

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
	public static final String USERNAME = "username";
	public static final String SOURCE = "source";
	public static final String ADDITIONAL_DATA = "additional_data";
	public static final String FIELDS = commaSeparated(ID, USERNAME, DATE, ELEMENT_TYPE, ELEMENT_SUB_TYPE,
			ELEMENT_ACCESSED, NEW_VERSION, ACTION, ACCESS_TYPE, SOURCE, ADDITIONAL_DATA);
	public static final String TABLE_NAME = "audits";

	private static final String querySizeSql = "select count(*) from audits";
	private static final String querySizeBetweenSql = "select count(*) from audits where date between ? and ?";
	private static final String queryAllBetweenSql = "select " + FIELDS + " from audits where date between ? and ?";
	private static final String queryAllByTypeAndBetweenSql = "select "
			+ FIELDS
			+ " from audits where element_type = ? and date between ? and ?";
	private static final String insertSql = "insert into audits ("
			+ FIELDS
			+ ") values (?, ?, ?, ?, ?, ?, ?, ?, ?::access_type, ?, ?)";

	private final PostgreSqlStrolchTransaction tx;

	public PostgreSqlAuditDao(PostgreSqlStrolchTransaction postgreSqlStrolchTransaction) {
		this.tx = postgreSqlStrolchTransaction;
	}

	@Override
	public long querySize() {
		try (PreparedStatement statement = this.tx.getConnection().prepareStatement(querySizeSql)) {
			try (ResultSet result = statement.executeQuery()) {
				result.next();
				return result.getLong(1);
			}
		} catch (SQLException e) {
			throw new StrolchPersistenceException("Failed to query size due to: " + e.getMessage(), e);
		}
	}

	@Override
	public long querySize(DateRange dateRange) {
		try (PreparedStatement statement = this.tx.getConnection().prepareStatement(querySizeBetweenSql)) {
			statement.setTimestamp(1, new Timestamp(dateRange.getFromDate().getTime()), Calendar.getInstance());
			statement.setTimestamp(2, new Timestamp(dateRange.getToDate().getTime()), Calendar.getInstance());

			try (ResultSet result = statement.executeQuery()) {
				result.next();
				return result.getLong(1);
			}

		} catch (SQLException e) {
			throw new StrolchPersistenceException("Failed to query size due to: " + e.getMessage(), e);
		}
	}

	@Override
	public List<Audit> queryAll(DateRange dateRange) {
		List<Audit> list = new ArrayList<>();
		try (PreparedStatement statement = this.tx.getConnection().prepareStatement(queryAllBetweenSql)) {
			statement.setTimestamp(1, new Timestamp(dateRange.getFromDate().getTime()), Calendar.getInstance());
			statement.setTimestamp(2, new Timestamp(dateRange.getToDate().getTime()), Calendar.getInstance());

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
	public List<Audit> queryAll(String type, DateRange dateRange) {
		List<Audit> list = new ArrayList<>();
		try (PreparedStatement statement = this.tx.getConnection().prepareStatement(queryAllByTypeAndBetweenSql)) {
			statement.setString(1, type);
			statement.setTimestamp(2, new Timestamp(dateRange.getFromDate().getTime()), Calendar.getInstance());
			statement.setTimestamp(3, new Timestamp(dateRange.getToDate().getTime()), Calendar.getInstance());

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
		try (PreparedStatement preparedStatement = this.tx.getConnection().prepareStatement(insertSql)) {
			setAuditFields(audit, preparedStatement);

			int count = preparedStatement.executeUpdate();
			if (count != 1) {
				throw new StrolchPersistenceException(
						MessageFormat.format("Expected to insert 1 record, but inserted {0} for audit {1}", count,
								audit.getId()));
			}

		} catch (SQLException e) {
			throw new StrolchPersistenceException(
					MessageFormat.format("Failed to insert Audit {0} due to {1}", audit, e.getLocalizedMessage()), e);
		}
	}

	@Override
	public void saveAll(List<Audit> audits) {
		for (Audit audit : audits) {
			save(audit);
		}
	}

	private void setAuditFields(Audit audit, PreparedStatement ps) throws SQLException {

		// 1  id = ?, 
		// 2  username = ?, 
		// 3  date = ?,
		// 4  element_type = ?,
		// 5  element_sub_type = ?,
		// 6  element_accessed = ?,
		// 7  new_version = ?,
		// 8 action = ?,
		// 9 access_type = ?::access_type,
		// 10 source = ?,
		// 11 additional_data

		ps.setLong(1, audit.getId());
		ps.setString(2, audit.getUsername());
		ps.setTimestamp(3, new Timestamp(audit.getDate().toInstant().toEpochMilli()), Calendar.getInstance());
		ps.setString(4, audit.getElementType());
		ps.setString(5, audit.getElementSubType());
		ps.setString(6, audit.getElementAccessed());

		if (audit.getNewVersion() == null)
			ps.setDate(7, null);
		else
			ps.setTimestamp(7, new Timestamp(audit.getNewVersion().toInstant().toEpochMilli()), Calendar.getInstance());

		ps.setString(8, audit.getAction());
		ps.setString(9, audit.getAccessType().name());
		ps.setString(10, audit.getAccessType().name());

		if (audit.getAdditionalDataAsString() == null) {
			ps.setObject(11, null);
		} else {
			PGobject pGobject = new PGobject();
			pGobject.setType("json");
			pGobject.setValue(audit.getAdditionalDataAsString());
			ps.setObject(11, pGobject);
		}
	}

	private Audit auditFrom(ResultSet resultSet) throws SQLException {

		Audit audit = new Audit();
		audit.setId(resultSet.getLong(1));
		audit.setUsername(resultSet.getString(2));
		audit.setDate(resultSet.getTimestamp(3).toInstant().atZone(ZoneId.systemDefault()));
		audit.setElementType(resultSet.getString(4));
		audit.setElementSubType(resultSet.getString(5));
		audit.setElementAccessed(resultSet.getString(6));
		Timestamp timestamp = resultSet.getTimestamp(7);
		if (timestamp != null)
			audit.setNewVersion(timestamp.toInstant().atZone(ZoneId.systemDefault()));
		audit.setAction(resultSet.getString(8));
		audit.setAccessType(AccessType.valueOf(resultSet.getString(9)));
		audit.setSource(resultSet.getString(10));

		PGobject pGobject = (PGobject) resultSet.getObject(11);
		if (pGobject != null) {
			String json = pGobject.getValue();
			if (json != null)
				audit.setAdditionalDataAsString(json);
		}
		return audit;
	}
}
