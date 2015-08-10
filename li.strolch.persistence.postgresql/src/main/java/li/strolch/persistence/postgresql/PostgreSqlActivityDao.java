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

import java.io.IOException;
import java.io.InputStream;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.SQLXML;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.List;

import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;
import javax.xml.transform.sax.SAXResult;

import li.strolch.model.Tags;
import li.strolch.model.activity.Activity;
import li.strolch.model.query.ActivityQuery;
import li.strolch.model.xml.ActivityToSaxVisitor;
import li.strolch.model.xml.SimpleStrolchElementListener;
import li.strolch.model.xml.XmlModelSaxReader;
import li.strolch.persistence.api.ActivityDao;
import li.strolch.persistence.api.StrolchPersistenceException;

import org.xml.sax.ContentHandler;
import org.xml.sax.SAXException;

@SuppressWarnings("nls")
public class PostgreSqlActivityDao extends PostgresqlDao<Activity> implements ActivityDao {

	public static final String ACTIVITIES = "activities";

	protected PostgreSqlActivityDao(PostgreSqlStrolchTransaction tx) {
		super(tx);
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
			throw new StrolchPersistenceException(MessageFormat.format(
					"Failed to extract Activity from sqlxml value for {0} / {1}", id, type), e);
		}

		if (listener.getActivities().size() == 0)
			throw new StrolchPersistenceException(MessageFormat.format(
					"No Activity parsed from sqlxml value for {0} / {1}", id, type));
		if (listener.getActivities().size() > 1)
			throw new StrolchPersistenceException(MessageFormat.format(
					"Multiple Activities parsed from sqlxml value for {0} / {1}", id, type));

		return listener.getActivities().get(0);
	}

	protected SQLXML createSqlXml(Activity activity, PreparedStatement preparedStatement) throws SQLException,
			SAXException {
		SQLXML sqlxml = tx().getConnection().createSQLXML();
		SAXResult saxResult = sqlxml.setResult(SAXResult.class);
		ContentHandler contentHandler = saxResult.getHandler();
		contentHandler.startDocument();
		new ActivityToSaxVisitor(contentHandler).visit(activity);
		contentHandler.endDocument();
		return sqlxml;
	}

	@Override
	protected void internalSave(final Activity activity) {
		String sql = "insert into " + getTableName() + " (id, name, type, asxml) values (?, ?, ?, ?)";
		try (PreparedStatement preparedStatement = tx().getConnection().prepareStatement(sql)) {
			preparedStatement.setString(1, activity.getId());
			preparedStatement.setString(2, activity.getName());
			preparedStatement.setString(3, activity.getType());

			SQLXML sqlxml = createSqlXml(activity, preparedStatement);
			preparedStatement.setSQLXML(4, sqlxml);
			try {
				int modCount = preparedStatement.executeUpdate();
				if (modCount != 1) {
					String msg = "Expected to save 1 element with id {0} but SQL statement modified {1} elements!";
					msg = MessageFormat.format(msg, activity.getId(), modCount);
					throw new StrolchPersistenceException(msg);
				}
			} finally {
				sqlxml.free();
			}

		} catch (SQLException | SAXException e) {
			throw new StrolchPersistenceException(MessageFormat.format("Failed to insert Activity {0} due to {1}",
					activity.getLocator(), e.getLocalizedMessage()), e);
		}
	}

	@Override
	protected void internalUpdate(final Activity activity) {
		String sql = "update " + getTableName() + " set name = ?, type = ?, asxml = ? where id = ? ";
		try (PreparedStatement preparedStatement = tx().getConnection().prepareStatement(sql)) {

			preparedStatement.setString(1, activity.getName());
			preparedStatement.setString(2, activity.getType());
			preparedStatement.setString(4, activity.getId());

			SQLXML sqlxml = createSqlXml(activity, preparedStatement);
			preparedStatement.setSQLXML(3, sqlxml);
			try {
				int modCount = preparedStatement.executeUpdate();
				if (modCount != 1) {
					String msg = "Expected to update 1 element with id {0} but SQL statement modified {1} elements!";
					msg = MessageFormat.format(msg, activity.getId(), modCount);
					throw new StrolchPersistenceException(msg);
				}
			} finally {
				sqlxml.free();
			}

		} catch (SQLException | SAXException e) {
			throw new StrolchPersistenceException(MessageFormat.format("Failed to update Activity {0} due to {1}",
					activity.getLocator(), e.getLocalizedMessage()), e);
		}
	}

	@Override
	public <U> List<U> doQuery(ActivityQuery<U> query) {

		PostgreSqlActivityQueryVisitor queryVisitor = new PostgreSqlActivityQueryVisitor("id, asxml");
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
					Activity t = parseFromXml(id, queryVisitor.getType(), sqlxml);
					list.add(query.getActivityVisitor().visit(t));
				}
			}
		} catch (SQLException e) {
			throw new StrolchPersistenceException("Failed to perform query due to: " + e.getMessage(), e);
		}

		return list;
	}
}
