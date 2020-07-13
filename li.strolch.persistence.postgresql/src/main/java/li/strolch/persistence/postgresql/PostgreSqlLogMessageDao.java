package li.strolch.persistence.postgresql;

import static li.strolch.utils.helper.StringHelper.commaSeparated;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.text.MessageFormat;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.*;

import li.strolch.model.log.LogMessage;
import li.strolch.model.log.LogMessageState;
import li.strolch.model.log.LogSeverity;
import li.strolch.model.Locator;
import li.strolch.persistence.api.LogMessageDao;
import li.strolch.persistence.api.StrolchPersistenceException;

public class PostgreSqlLogMessageDao implements LogMessageDao {

	private static final String ID = "id";
	private static final String REALM = "realm";
	private static final String DATE_TIME = "dateTime";
	private static final String USERNAME = "username";
	private static final String SEVERITY = "severity";
	private static final String LOCATOR = "locator";
	private static final String KEY = "key";
	private static final String MESSAGE = "message";
	private static final String STACK_TRACE = "stacktrace";
	private static final String STATE = "state";

	private static final String FIELDS = commaSeparated(ID, REALM, DATE_TIME, USERNAME, SEVERITY, STATE, LOCATOR, KEY,
			MESSAGE, STACK_TRACE);

	private static final String queryByRealmMaxSql =
			"select " + FIELDS + " from operations_log where realm = ? order by id desc limit ?";
	private static final String queryValuesSql = "select key, value from operations_log_values where id = ?";

	private static final String insertLogMessageSql = "insert into operations_log (" + FIELDS
			+ ") values (?, ?, ?, ?, ?::log_severity_type, ?::log_state_type, ?, ?, ?, ?)";
	private static final String insertValuesSql = "insert into operations_log_values (id, key, value) values (?, ?, ?)";

	private static final String updateLogMessageStateSql = "update operations_log set state = ?::log_state_type where id = ?";

	private static final String removeSql = "delete from operations_log where id = ?";
	private static final String removeValuesSql = "delete from operations_log_values where id = ?";

	private final PostgreSqlStrolchTransaction tx;

	public PostgreSqlLogMessageDao(PostgreSqlStrolchTransaction postgreSqlStrolchTransaction) {
		this.tx = postgreSqlStrolchTransaction;
	}

	@Override
	public List<LogMessage> queryLatest(String realm, int maxNr) {

		try (PreparedStatement queryMsgStatement = this.tx.getConnection().prepareStatement(queryByRealmMaxSql);
				PreparedStatement queryValuesStatement = this.tx.getConnection().prepareStatement(queryValuesSql)) {

			queryMsgStatement.setString(1, realm);
			queryMsgStatement.setInt(2, maxNr);

			List<LogMessage> messages = new ArrayList<>();

			try (ResultSet result = queryMsgStatement.executeQuery()) {
				while (result.next()) {

					String id = result.getString(1);
					queryValuesStatement.setString(1, id);

					try (ResultSet valuesResult = queryValuesStatement.executeQuery()) {
						messages.add(logMessageFrom(result, valuesResult));
					}
				}
			}

			return messages;

		} catch (SQLException e) {
			throw new StrolchPersistenceException("Failed to query types due to: " + e.getMessage(), e); //$NON-NLS-1$
		}
	}

	@Override
	public void save(LogMessage logMessage) {
		try (PreparedStatement insertStatement = this.tx.getConnection().prepareStatement(insertLogMessageSql);
				PreparedStatement valuesStatement = this.tx.getConnection().prepareStatement(insertValuesSql)) {

			// insert log message
			setLogMessageFields(logMessage, insertStatement);
			int count = insertStatement.executeUpdate();
			if (count != 1) {
				throw new StrolchPersistenceException(MessageFormat
						.format("Expected to insert 1 log_message record, but inserted {0} for LogMessage {1}", count,
								logMessage.getId())); //$NON-NLS-1$
			}

			int nrOfInserts = setValues(logMessage, valuesStatement);
			int[] ints = valuesStatement.executeBatch();
			validateValuesStatement(logMessage, nrOfInserts, ints);

		} catch (SQLException e) {
			throw new StrolchPersistenceException(
					MessageFormat.format("Failed to insert LogMessage {0} due to {1}", logMessage.getId(), //$NON-NLS-1$
							e.getLocalizedMessage()), e);
		}
	}

	@Override
	public void saveAll(List<LogMessage> logMessages) {
		logMessages.forEach(this::save);
	}

	@Override
	public void updateState(LogMessage logMessage) {
		try (PreparedStatement ps = this.tx.getConnection().prepareStatement(updateLogMessageStateSql)) {

			// update state
			ps.setString(1, logMessage.getState().name());
			ps.setString(2, logMessage.getId());

			// we ignore the number of updates, as the message might have been deleted meanwhile
			ps.executeUpdate();

		} catch (SQLException e) {
			throw new StrolchPersistenceException(MessageFormat
					.format("Failed to update LogMessage state {0} due to {1}", logMessage.getId(), //$NON-NLS-1$
							e.getLocalizedMessage()), e);
		}
	}

	@Override
	public void updateStates(Collection<LogMessage> logMessages) {

		try (PreparedStatement ps = this.tx.getConnection().prepareStatement(updateLogMessageStateSql)) {

			// update state
			for (LogMessage logMessage : logMessages) {
				ps.setString(1, logMessage.getState().name());
				ps.setString(2, logMessage.getId());
				ps.addBatch();
			}

			// we ignore the number of updates, as the message might have been deleted meanwhile
			ps.executeUpdate();

		} catch (SQLException e) {
			throw new StrolchPersistenceException(MessageFormat
					.format("Failed to update states for {0} LogMessages due to {1}", logMessages.size(),
							e.getLocalizedMessage()), e);
		}
	}

	@Override
	public void remove(LogMessage logMessage) {
		try (PreparedStatement removeStatement = this.tx.getConnection().prepareStatement(removeSql);
				PreparedStatement removeValuesStatement = this.tx.getConnection().prepareStatement(removeValuesSql)) {

			remove(removeStatement, removeValuesStatement, logMessage);

		} catch (SQLException e) {
			throw new StrolchPersistenceException(MessageFormat.format("Failed to remove {0} due to {1}", //$NON-NLS-1$
					logMessage.getId(), e.getLocalizedMessage()), e);
		}
	}

	@Override
	public void removeAll(List<LogMessage> logMessages) {
		try (PreparedStatement removeStatement = this.tx.getConnection().prepareStatement(removeSql);
				PreparedStatement removeValuesStatement = this.tx.getConnection().prepareStatement(removeValuesSql)) {

			int nrOfRemoves = 0;
			int[] nrOfValueRemoves = new int[logMessages.size()];
			for (LogMessage logMessage : logMessages) {

				removeStatement.setString(1, logMessage.getId());
				removeValuesStatement.setString(1, logMessage.getId());

				removeStatement.addBatch();
				removeValuesStatement.addBatch();
				nrOfValueRemoves[nrOfRemoves] = logMessage.getValues().size();
				nrOfRemoves++;
			}

			int[] countAll = removeStatement.executeBatch();
			if (countAll.length != nrOfRemoves) {
				String msg = "Expected to delete {0} LogMessages but deleted {1} elements!"; //$NON-NLS-1$
				msg = MessageFormat.format(msg, nrOfRemoves, countAll.length);
				throw new StrolchPersistenceException(msg);
			}
			for (int count : countAll) {
				if (count != 1) {
					String msg = "Expected to delete 1 LogMessages per delete statement but deleted {0} elements!"; //$NON-NLS-1$
					msg = MessageFormat.format(msg, nrOfRemoves, count);
					throw new StrolchPersistenceException(msg);
				}
			}

			countAll = removeValuesStatement.executeBatch();
			if (countAll.length != nrOfRemoves) {
				String msg = "Expected to execute {0} delete value statements but executed {1} elements!"; //$NON-NLS-1$
				msg = MessageFormat.format(msg, nrOfRemoves, countAll.length);
				throw new StrolchPersistenceException(msg);
			}
			for (int i = 0; i < countAll.length; i++) {
				if (countAll[i] != nrOfValueRemoves[i]) {
					String msg = "Expected to delete {0} values for LogMessage {1} but deleted {2} elements!"; //$NON-NLS-1$
					msg = MessageFormat.format(msg, nrOfValueRemoves[i], logMessages.get(i).getId(), countAll[i]);
					throw new StrolchPersistenceException(msg);
				}
			}

		} catch (SQLException e) {
			throw new StrolchPersistenceException(
					MessageFormat.format("Failed to remove LogMessages due to {0}", e.getLocalizedMessage()), e);
		}
	}

	private void remove(PreparedStatement removeStatement, PreparedStatement removeValuesStatement,
			LogMessage logMessage) throws SQLException {

		removeStatement.setString(1, logMessage.getId());
		removeValuesStatement.setString(1, logMessage.getId());

		int count = removeStatement.executeUpdate();
		if (count != 1) {
			String msg = "Expected to delete 1 LogMessage with id {0} but deleted {1} elements!"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, logMessage.getId(), count);
			throw new StrolchPersistenceException(msg);
		}

		count = removeValuesStatement.executeUpdate();
		if (count != logMessage.getValues().size()) {
			String msg = "Expected to delete {0} values for LogMessage with id {1} but deleted {2} elements!"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, logMessage.getValues().size(), logMessage.getId(), count);
			throw new StrolchPersistenceException(msg);
		}
	}

	private void validateValuesStatement(LogMessage logMessage, int nrOfInserts, int[] ints) {
		if (ints.length != nrOfInserts) {
			throw new StrolchPersistenceException(MessageFormat
					.format("Expected to insert {0} value record, but inserted {1} for LogMessage {2}", nrOfInserts,
							ints.length, logMessage.getId())); //$NON-NLS-1$
		}

		for (int i = 0; i < ints.length; i++) {
			if (ints[i] != 1) {
				throw new StrolchPersistenceException(MessageFormat
						.format("Expected to insert 1 record per value, but inserted {0} for value at index {1} for LogMessage {2}",
								ints[i], i, logMessage.getId())); //$NON-NLS-1$
			}
		}
	}

	private int setValues(LogMessage logMessage, PreparedStatement valuesStatement) throws SQLException {
		// insert properties
		Properties values = logMessage.getValues();
		int nrOfInserts = 0;
		Set<String> keys = values.stringPropertyNames();
		for (String key : keys) {

			valuesStatement.setString(1, logMessage.getId());
			valuesStatement.setString(2, key);
			valuesStatement.setString(3, values.getProperty(key));

			valuesStatement.addBatch();
			nrOfInserts++;
		}
		return nrOfInserts;
	}

	private void setLogMessageFields(LogMessage logMessage, PreparedStatement ps) throws SQLException {

		// 1  id = ?,
		// 2  realm = ?,
		// 3  dateTime = ?,
		// 4  username = ?,
		// 5  severity = ?,
		// 6 state = ?
		// 7  locator = ?,
		// 8  key = ?,
		// 9  message = ?,
		// 10  stacktrace = ?,

		ps.setString(1, logMessage.getId());
		ps.setString(2, logMessage.getRealm());
		ps.setTimestamp(3, new Timestamp(logMessage.getZonedDateTime().toInstant().toEpochMilli()),
				Calendar.getInstance());
		ps.setString(4, logMessage.getUsername());
		ps.setString(5, logMessage.getSeverity().name());
		ps.setString(6, logMessage.getState().name());
		ps.setString(7, logMessage.getLocator().toString());
		ps.setString(8, logMessage.getKey());
		ps.setString(9, logMessage.getMessage());
		ps.setString(10, logMessage.getStackTrace());
	}

	private LogMessage logMessageFrom(ResultSet resultSet, ResultSet valuesResult) throws SQLException {

		String id = resultSet.getString(1);
		String realm = resultSet.getString(2);
		ZonedDateTime dateTime = ZonedDateTime.ofInstant(resultSet.getTimestamp(3).toInstant(), ZoneId.systemDefault());
		String username = resultSet.getString(4);
		LogSeverity severity = LogSeverity.valueOf(resultSet.getString(5));
		LogMessageState state = LogMessageState.valueOf(resultSet.getString(6));
		Locator locator = Locator.valueOf(resultSet.getString(7));
		String key = resultSet.getString(8);
		String message = resultSet.getString(9);
		String exception = resultSet.getString(10);

		Properties properties = new Properties();
		while (valuesResult.next()) {
			String valueK = valuesResult.getString(1);
			String valueV = valuesResult.getString(2);
			properties.setProperty(valueK, valueV);
		}

		return new LogMessage(id, dateTime, realm, username, locator, severity, state, key, properties, message,
				exception);
	}
}
