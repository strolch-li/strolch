/*
 * Copyright 2015 Robert von Burg <eitch@eitchnet.ch>
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
import li.strolch.model.Tags;
import li.strolch.model.activity.Activity;
import li.strolch.model.json.ActivityFromJsonVisitor;
import li.strolch.model.xml.SimpleStrolchElementListener;
import li.strolch.model.xml.XmlModelSaxReader;
import li.strolch.persistence.api.ActivityDao;
import li.strolch.persistence.api.StrolchPersistenceException;
import li.strolch.persistence.api.TransactionResult;
import org.xml.sax.SAXException;

@SuppressWarnings("nls")
public class PostgreSqlActivityDao extends PostgresqlDao<Activity> implements ActivityDao {

	public static final String ACTIVITIES = "activities";

	private static final String insertAsXmlSqlS = "insert into {0} (id, version, created_by, created_at, updated_at, deleted, latest, name, type, state, asxml) values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?::order_state, ?)";
	private static final String insertAsJsonSqlS = "insert into {0} (id, version, created_by, created_at, updated_at, deleted, latest, name, type, state, asjson) values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?::order_state, ?)";

	private static final String updateAsXmlSqlS = "update {0} set created_by = ?, created_at = ?, updated_at = ?, deleted = ?, latest = ?, name = ?, type = ?, state = ?::order_state, asxml = ? where id = ? and version = ?";
	private static final String updateAsJsonSqlS = "update {0} set created_by = ?, created_at = ?, updated_at = ?, deleted = ?, latest = ?, name = ?, type = ?, state = ?::order_state, asjson = ? where id = ? and version = ?";

	private static final String updateLatestSqlS = "update {0} SET latest = false WHERE id = ? AND version = ?";

	public PostgreSqlActivityDao(DataType dataType, Connection connection, TransactionResult txResult,
			boolean versioningEnabled) {
		super(dataType, connection, txResult, versioningEnabled);
	}

	@Override
	protected String getClassName() {
		return Tags.ACTIVITY;
	}

	@Override
	protected String getTableName() {
		return ACTIVITIES;
	}

	@Override
	protected Activity parseFromXml(String id, String type, SQLXML sqlxml) {
		SimpleStrolchElementListener listener = new SimpleStrolchElementListener();
		try (InputStream binaryStream = sqlxml.getBinaryStream()) {
			SAXParser parser = SAXParserFactory.newInstance().newSAXParser();
			parser.parse(binaryStream, new XmlModelSaxReader(listener));
		} catch (SQLException | IOException | SAXException | ParserConfigurationException e) {
			throw new StrolchPersistenceException(
					MessageFormat.format("Failed to extract Activity from sqlxml value for {0} / {1}", id, type), e);
		}

		if (listener.getActivities().size() == 0)
			throw new StrolchPersistenceException(
					MessageFormat.format("No Activity parsed from sqlxml value for {0} / {1}", id, type));
		if (listener.getActivities().size() > 1)
			throw new StrolchPersistenceException(
					MessageFormat.format("Multiple Activities parsed from sqlxml value for {0} / {1}", id, type));

		return listener.getActivities().get(0);
	}

	@Override
	protected Activity parseFromJson(String id, String type, String json) {
		JsonObject jsonObject = new JsonParser().parse(json).getAsJsonObject();
		return new ActivityFromJsonVisitor().visit(jsonObject);
	}

	@Override
	protected void internalSave(final Activity activity) {

		String sql = getSql(insertAsXmlSqlS, insertAsJsonSqlS);

		try (PreparedStatement preparedStatement = this.connection.prepareStatement(sql)) {

			// id
			preparedStatement.setString(1, activity.getId());

			// version
			preparedStatement.setInt(2, activity.getVersion().getVersion());
			preparedStatement.setString(3, activity.getVersion().getCreatedBy());
			preparedStatement.setTimestamp(4, new Timestamp(activity.getVersion().getCreated().getTime()),
					Calendar.getInstance());
			preparedStatement.setTimestamp(5, new Timestamp(activity.getVersion().getUpdated().getTime()),
					Calendar.getInstance());
			preparedStatement.setBoolean(6, activity.getVersion().isDeleted());

			preparedStatement.setBoolean(7, !activity.getVersion().isDeleted());

			// attributes
			preparedStatement.setString(8, activity.getName());
			preparedStatement.setString(9, activity.getType());
			preparedStatement.setString(10, activity.getState().name());

			SQLXML sqlxml = writeObject(preparedStatement, activity, 11);

			try {
				int modCount = preparedStatement.executeUpdate();
				if (modCount != 1) {
					String msg = "Expected to save 1 element with id {0} but SQL statement modified {1} elements!";
					msg = MessageFormat.format(msg, activity.getId(), modCount);
					throw new StrolchPersistenceException(msg);
				}
			} finally {
				if (sqlxml != null)
					sqlxml.free();
			}

		} catch (SQLException | SAXException e) {
			throw new StrolchPersistenceException(MessageFormat
					.format("Failed to insert Activity {0} due to {1}", activity.getLocator(), e.getLocalizedMessage()),
					e);
		}

		if (activity.getVersion().isFirstVersion()) {
			return;
		}

		// and set the previous version to not be latest anymore
		sql = MessageFormat.format(updateLatestSqlS, getTableName());
		try (PreparedStatement preparedStatement = this.connection.prepareStatement(sql)) {

			// primary key
			preparedStatement.setString(1, activity.getId());
			preparedStatement.setInt(2, activity.getVersion().getPreviousVersion());

			int modCount = preparedStatement.executeUpdate();
			if (modCount != 1) {
				String msg = "Expected to update 1 previous element with id {0} and version {1} but SQL statement modified {2} elements!";
				msg = MessageFormat.format(msg, activity.getId(), activity.getVersion().getPreviousVersion(), modCount);
				throw new StrolchPersistenceException(msg);
			}

		} catch (SQLException e) {
			throw new StrolchPersistenceException(MessageFormat
					.format("Failed to update previous version of Activity {0} due to {1}", activity.getVersion(),
							e.getLocalizedMessage()), e);
		}
	}

	@Override
	protected void internalUpdate(final Activity activity) {

		// with versioning we save a new object
		if (this.versioningEnabled) {
			internalSave(activity);
			return;
		}

		// make sure is first version when versioning is not enabled
		if (!activity.getVersion().isFirstVersion()) {
			throw new StrolchPersistenceException(MessageFormat
					.format("Versioning is not enabled, so version must always be 0 to perform an update, but it is {0}",
							activity.getVersion()));
		}

		// and also not marked as deleted!
		if (activity.getVersion().isDeleted()) {
			throw new StrolchPersistenceException(MessageFormat
					.format("Versioning is not enabled, so version can not be marked as deleted for {0}",
							activity.getVersion()));
		}

		String sql = getSql(updateAsXmlSqlS, updateAsJsonSqlS);

		try (PreparedStatement preparedStatement = this.connection.prepareStatement(sql)) {

			// version
			preparedStatement.setString(1, activity.getVersion().getCreatedBy());
			preparedStatement.setTimestamp(2, new Timestamp(activity.getVersion().getCreated().getTime()),
					Calendar.getInstance());
			preparedStatement.setTimestamp(3, new Timestamp(activity.getVersion().getUpdated().getTime()),
					Calendar.getInstance());
			preparedStatement.setBoolean(4, activity.getVersion().isDeleted());

			preparedStatement.setBoolean(5, !activity.getVersion().isDeleted());

			// attributes
			preparedStatement.setString(6, activity.getName());
			preparedStatement.setString(7, activity.getType());
			preparedStatement.setString(8, activity.getState().name());

			SQLXML sqlxml = writeObject(preparedStatement, activity, 9);

			// primary key
			preparedStatement.setString(10, activity.getId());
			preparedStatement.setInt(11, activity.getVersion().getVersion());

			try {
				int modCount = preparedStatement.executeUpdate();
				if (modCount != 1) {
					String msg = "Expected to update 1 element with id {0} but SQL statement modified {1} elements!";
					msg = MessageFormat.format(msg, activity.getId(), modCount);
					throw new StrolchPersistenceException(msg);
				}
			} finally {
				if (sqlxml != null)
					sqlxml.free();
			}

		} catch (SQLException | SAXException e) {
			throw new StrolchPersistenceException(MessageFormat
					.format("Failed to update Activity {0} due to {1}", activity.getLocator(), e.getLocalizedMessage()),
					e);
		}
	}
}
