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
import li.strolch.model.Resource;
import li.strolch.model.Tags;
import li.strolch.model.json.ResourceFromJsonVisitor;
import li.strolch.model.xml.SimpleStrolchElementListener;
import li.strolch.model.xml.XmlModelSaxReader;
import li.strolch.persistence.api.ResourceDao;
import li.strolch.persistence.api.StrolchPersistenceException;
import li.strolch.persistence.api.TransactionResult;
import org.xml.sax.SAXException;

@SuppressWarnings("nls")
public class PostgreSqlResourceDao extends PostgresqlDao<Resource> implements ResourceDao {

	public static final String RESOURCES = "resources";

	private static final String insertAsXmlSqlS = "insert into {0} (id, version, created_by, updated_at, created_at, deleted, latest, name, type, asxml) values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)";
	private static final String insertAsJsonSqlS = "insert into {0} (id, version, created_by, updated_at, created_at, deleted, latest, name, type, asjson) values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)";

	private static final String updateAsXmlSqlS = "update {0} set created_by = ?, created_at = ?, updated_at = ?, deleted = ?, latest = ?, name = ?, type = ?, asxml = ? where id = ? and version = ?";
	private static final String updateAsJsonSqlS = "update {0} set created_by = ?, created_at = ?, updated_at = ?, deleted = ?, latest = ?, name = ?, type = ?, asjson = ? where id = ? and version = ?";

	private static final String updateLatestSqlS = "update {0} SET latest = false WHERE id = ? AND version = ?";

	protected PostgreSqlResourceDao(DataType dataType, Connection connection, TransactionResult txResult,
			boolean versioningEnabled) {
		super(dataType, connection, txResult, versioningEnabled);
	}

	@Override
	protected String getClassName() {
		return Tags.RESOURCE;
	}

	@Override
	protected String getTableName() {
		return RESOURCES;
	}

	@Override
	protected Resource parseFromXml(String id, String type, SQLXML sqlxml) {
		SimpleStrolchElementListener listener = new SimpleStrolchElementListener();
		try (InputStream binaryStream = sqlxml.getBinaryStream()) {
			SAXParser parser = SAXParserFactory.newInstance().newSAXParser();
			parser.parse(binaryStream, new XmlModelSaxReader(listener));
		} catch (SQLException | IOException | SAXException | ParserConfigurationException e) {
			throw new StrolchPersistenceException(
					MessageFormat.format("Failed to extract Resource from sqlxml value for {0} / {1}", id, type), e);
		}

		if (listener.getResources().size() == 0)
			throw new StrolchPersistenceException(
					MessageFormat.format("No Resource parsed from sqlxml value for {0} / {1}", id, type));
		if (listener.getResources().size() > 1)
			throw new StrolchPersistenceException(
					MessageFormat.format("Multiple Resources parsed from sqlxml value for {0} / {1}", id, type));

		return listener.getResources().get(0);
	}

	@Override
	protected Resource parseFromJson(String id, String type, String json) {
		JsonObject jsonObject = new JsonParser().parse(json).getAsJsonObject();
		return new ResourceFromJsonVisitor().visit(jsonObject);
	}

	@Override
	protected void internalSave(final Resource resource) {

		String sql = getSql(insertAsXmlSqlS, insertAsJsonSqlS);

		try (PreparedStatement preparedStatement = this.connection.prepareStatement(sql)) {

			// id
			preparedStatement.setString(1, resource.getId());

			// version
			preparedStatement.setInt(2, resource.getVersion().getVersion());
			preparedStatement.setString(3, resource.getVersion().getCreatedBy());
			preparedStatement.setTimestamp(4, new Timestamp(resource.getVersion().getCreated().getTime()),
					Calendar.getInstance());
			preparedStatement.setTimestamp(5, new Timestamp(resource.getVersion().getUpdated().getTime()),
					Calendar.getInstance());
			preparedStatement.setBoolean(6, resource.getVersion().isDeleted());

			preparedStatement.setBoolean(7, !resource.getVersion().isDeleted());

			// attributes
			preparedStatement.setString(8, resource.getName());
			preparedStatement.setString(9, resource.getType());

			SQLXML sqlxml = writeObject(preparedStatement, resource, 10);

			try {
				int modCount = preparedStatement.executeUpdate();
				if (modCount != 1) {
					String msg = "Expected to save 1 element with id {0} but SQL statement modified {1} elements!";
					msg = MessageFormat.format(msg, resource.getId(), modCount);
					throw new StrolchPersistenceException(msg);
				}
			} finally {
				if (sqlxml != null)
					sqlxml.free();
			}

		} catch (SQLException | SAXException e) {
			throw new StrolchPersistenceException(MessageFormat
					.format("Failed to insert Resource {0} due to {1}", resource.getLocator(), e.getLocalizedMessage()),
					e);
		}

		if (resource.getVersion().isFirstVersion()) {
			return;
		}

		// and set the previous version to not be latest anymore
		sql = MessageFormat.format(updateLatestSqlS, getTableName());
		try (PreparedStatement preparedStatement = this.connection.prepareStatement(sql)) {

			// primary key
			preparedStatement.setString(1, resource.getId());
			preparedStatement.setInt(2, resource.getVersion().getPreviousVersion());

			int modCount = preparedStatement.executeUpdate();
			if (modCount != 1) {
				String msg = "Expected to update 1 previous element with id {0} and version {1} but SQL statement modified {2} elements!";
				msg = MessageFormat.format(msg, resource.getId(), resource.getVersion().getPreviousVersion(), modCount);
				throw new StrolchPersistenceException(msg);
			}

		} catch (SQLException e) {
			throw new StrolchPersistenceException(MessageFormat
					.format("Failed to insert Resource {0} due to {1}", resource.getLocator(), e.getLocalizedMessage()),
					e);
		}
	}

	@Override
	protected void internalUpdate(final Resource resource) {

		// with versioning we save a new object
		if (this.versioningEnabled) {
			internalSave(resource);
			return;
		}

		// make sure is first version when versioning is not enabled
		if (!resource.getVersion().isFirstVersion()) {
			throw new StrolchPersistenceException(MessageFormat
					.format("Versioning is not enabled, so version must always be 0 to perform an update, but it is {0}",
							resource.getVersion()));
		}

		// and also not marked as deleted!
		if (resource.getVersion().isDeleted()) {
			throw new StrolchPersistenceException(MessageFormat
					.format("Versioning is not enabled, so version can not be marked as deleted for {0}",
							resource.getVersion()));
		}

		// now we update the existing object
		String sql = getSql(updateAsXmlSqlS, updateAsJsonSqlS);

		try (PreparedStatement preparedStatement = this.connection.prepareStatement(sql)) {

			// version
			preparedStatement.setString(1, resource.getVersion().getCreatedBy());
			preparedStatement.setTimestamp(2, new Timestamp(resource.getVersion().getCreated().getTime()),
					Calendar.getInstance());
			preparedStatement.setTimestamp(3, new Timestamp(resource.getVersion().getUpdated().getTime()),
					Calendar.getInstance());
			preparedStatement.setBoolean(4, resource.getVersion().isDeleted());

			preparedStatement.setBoolean(5, !resource.getVersion().isDeleted());

			// attributes
			preparedStatement.setString(6, resource.getName());
			preparedStatement.setString(7, resource.getType());

			SQLXML sqlxml = writeObject(preparedStatement, resource, 8);

			// primary key
			preparedStatement.setString(9, resource.getId());
			preparedStatement.setInt(10, resource.getVersion().getVersion());

			try {
				int modCount = preparedStatement.executeUpdate();
				if (modCount != 1) {
					String msg = "Expected to update 1 element with id {0} and version {1} but SQL statement modified {2} elements!";
					msg = MessageFormat.format(msg, resource.getId(), resource.getVersion().getVersion(), modCount);
					throw new StrolchPersistenceException(msg);
				}
			} finally {
				if (sqlxml != null)
					sqlxml.free();
			}

		} catch (SQLException | SAXException e) {
			throw new StrolchPersistenceException(MessageFormat
					.format("Failed to update Resource {0} due to {1}", resource.getLocator(), e.getLocalizedMessage()),
					e);
		}
	}
}
