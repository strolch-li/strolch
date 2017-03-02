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

import java.io.IOException;
import java.io.InputStream;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.SQLXML;
import java.sql.Timestamp;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;

import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;
import javax.xml.transform.sax.SAXResult;

import org.xml.sax.ContentHandler;
import org.xml.sax.SAXException;

import li.strolch.model.Resource;
import li.strolch.model.Tags;
import li.strolch.model.query.ResourceQuery;
import li.strolch.model.xml.SimpleStrolchElementListener;
import li.strolch.model.xml.StrolchElementToSaxVisitor;
import li.strolch.model.xml.XmlModelSaxReader;
import li.strolch.persistence.api.ResourceDao;
import li.strolch.persistence.api.StrolchPersistenceException;

@SuppressWarnings("nls")
public class PostgreSqlResourceDao extends PostgresqlDao<Resource> implements ResourceDao {

	public static final String RESOURCES = "resources";

	protected PostgreSqlResourceDao(PostgreSqlStrolchTransaction tx) {
		super(tx);
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

	protected SQLXML createSqlXml(Resource res, PreparedStatement preparedStatement) throws SQLException, SAXException {
		SQLXML sqlxml = tx().getConnection().createSQLXML();
		SAXResult saxResult = sqlxml.setResult(SAXResult.class);
		ContentHandler contentHandler = saxResult.getHandler();
		contentHandler.startDocument();
		res.accept(new StrolchElementToSaxVisitor(contentHandler));
		contentHandler.endDocument();
		return sqlxml;
	}

	@Override
	protected void internalSave(final Resource res) {
		String sql = "insert into " + getTableName()
				+ " (id, version, created_by, created_at, deleted, latest, name, type, asxml) values (?, ?, ?, ?, ?, ?, ?, ?, ?)";
		try (PreparedStatement preparedStatement = tx().getConnection().prepareStatement(sql)) {

			// id
			preparedStatement.setString(1, res.getId());

			// version
			preparedStatement.setInt(2, res.getVersion().getVersion());
			preparedStatement.setString(3, res.getVersion().getCreatedBy());
			preparedStatement.setTimestamp(4, new Timestamp(res.getVersion().getCreatedAt().getTime()),
					Calendar.getInstance());
			preparedStatement.setBoolean(5, res.getVersion().isDeleted());

			preparedStatement.setBoolean(6, !res.getVersion().isDeleted());

			// attributes
			preparedStatement.setString(7, res.getName());
			preparedStatement.setString(8, res.getType());

			SQLXML sqlxml = createSqlXml(res, preparedStatement);
			preparedStatement.setSQLXML(9, sqlxml);
			try {
				int modCount = preparedStatement.executeUpdate();
				if (modCount != 1) {
					String msg = "Expected to save 1 element with id {0} but SQL statement modified {1} elements!";
					msg = MessageFormat.format(msg, res.getId(), modCount);
					throw new StrolchPersistenceException(msg);
				}
			} finally {
				sqlxml.free();
			}

		} catch (SQLException | SAXException e) {
			throw new StrolchPersistenceException(MessageFormat.format("Failed to insert Resource {0} due to {1}",
					res.getLocator(), e.getLocalizedMessage()), e);
		}

		if (res.getVersion().isFirstVersion()) {
			return;
		}

		// and set the previous version to not be latest anymore
		sql = "update " + getTableName() + " SET latest = false WHERE id = ? AND version = ?";
		try (PreparedStatement preparedStatement = tx().getConnection().prepareStatement(sql)) {

			// primary key
			preparedStatement.setString(1, res.getId());
			preparedStatement.setInt(2, res.getVersion().getPreviousVersion());

			int modCount = preparedStatement.executeUpdate();
			if (modCount != 1) {
				String msg = "Expected to update 1 previous element with id {0} and version {1} but SQL statement modified {2} elements!";
				msg = MessageFormat.format(msg, res.getId(), res.getVersion().getPreviousVersion(), modCount);
				throw new StrolchPersistenceException(msg);
			}

		} catch (SQLException e) {
			throw new StrolchPersistenceException(MessageFormat.format("Failed to insert Resource {0} due to {1}",
					res.getLocator(), e.getLocalizedMessage()), e);
		}
	}

	@Override
	protected void internalUpdate(final Resource resource) {

		// with versioning we save a new object
		if (tx().getRealm().isVersioningEnabled()) {
			internalSave(resource);
			return;
		}

		// make sure is first version when versioning is not enabled
		if (!resource.getVersion().isFirstVersion()) {
			throw new StrolchPersistenceException(MessageFormat.format(
					"Versioning is not enabled, so version must always be 0 to perform an update, but it is {0}",
					resource.getVersion()));
		}

		// and also not marked as deleted!
		if (resource.getVersion().isDeleted()) {
			throw new StrolchPersistenceException(
					MessageFormat.format("Versioning is not enabled, so version can not be marked as deleted for {0}",
							resource.getVersion()));
		}

		// now we update the existing object
		String sql = "update " + getTableName()
				+ " set created_by = ?, created_at = ?, deleted = ?, latest = ?, name = ?, type = ?, asxml = ? where id = ? and version = ?";
		try (PreparedStatement preparedStatement = tx().getConnection().prepareStatement(sql)) {

			// version
			preparedStatement.setString(1, resource.getVersion().getCreatedBy());
			preparedStatement.setTimestamp(2, new Timestamp(resource.getVersion().getCreatedAt().getTime()),
					Calendar.getInstance());
			preparedStatement.setBoolean(3, resource.getVersion().isDeleted());

			preparedStatement.setBoolean(4, !resource.getVersion().isDeleted());

			// attributes
			preparedStatement.setString(5, resource.getName());
			preparedStatement.setString(6, resource.getType());

			SQLXML sqlxml = createSqlXml(resource, preparedStatement);
			preparedStatement.setSQLXML(7, sqlxml);

			// primary key
			preparedStatement.setString(8, resource.getId());
			preparedStatement.setInt(9, resource.getVersion().getVersion());

			try {
				int modCount = preparedStatement.executeUpdate();
				if (modCount != 1) {
					String msg = "Expected to update 1 element with id {0} and version {1} but SQL statement modified {2} elements!";
					msg = MessageFormat.format(msg, resource.getId(), resource.getVersion().getVersion(), modCount);
					throw new StrolchPersistenceException(msg);
				}
			} finally {
				sqlxml.free();
			}

		} catch (SQLException | SAXException e) {
			throw new StrolchPersistenceException(MessageFormat.format("Failed to update Resource {0} due to {1}",
					resource.getLocator(), e.getLocalizedMessage()), e);
		}
	}

	@Override
	public <U> List<U> doQuery(ResourceQuery<U> query) {

		PostgreSqlResourceQueryVisitor queryVisitor = new PostgreSqlResourceQueryVisitor("id, asxml");
		query.accept(queryVisitor);
		queryVisitor.validate();

		List<U> list = new ArrayList<>();

		String sql = queryVisitor.getSql();
		try (PreparedStatement ps = tx().getConnection().prepareStatement(sql)) {
			queryVisitor.setValues(ps);

			try (ResultSet result = ps.executeQuery()) {
				while (result.next()) {
					String id = result.getString("id");
					SQLXML sqlxml = result.getSQLXML("asxml");
					Resource t = parseFromXml(id, queryVisitor.getType(), sqlxml);
					list.add(query.getResourceVisitor().visit(t));
				}
			}
		} catch (SQLException e) {
			throw new StrolchPersistenceException("Failed to perform query due to: " + e.getMessage(), e);
		}

		return list;
	}
}
