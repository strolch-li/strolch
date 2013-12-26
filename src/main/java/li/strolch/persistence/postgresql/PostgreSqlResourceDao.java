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
import java.sql.SQLException;
import java.sql.SQLXML;
import java.text.MessageFormat;
import java.util.List;

import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;
import javax.xml.transform.sax.SAXResult;

import li.strolch.model.Resource;
import li.strolch.model.Tags;
import li.strolch.model.query.ResourceQuery;
import li.strolch.model.xml.ResourceToSaxVisitor;
import li.strolch.model.xml.SimpleStrolchElementListener;
import li.strolch.model.xml.XmlModelDefaultHandler;
import li.strolch.persistence.api.ResourceDao;
import li.strolch.persistence.api.StrolchPersistenceException;

import org.xml.sax.ContentHandler;
import org.xml.sax.SAXException;

@SuppressWarnings("nls")
public class PostgreSqlResourceDao extends PostgresqlDao<Resource> implements ResourceDao {

	protected PostgreSqlResourceDao(PostgreSqlStrolchTransaction tx) {
		super(tx);
	}

	@Override
	protected String getClassName() {
		return Tags.RESOURCE;
	}

	@Override
	protected String getTableName() {
		return "resources";
	}

	@Override
	protected Resource parseFromXml(String id, String type, SQLXML sqlxml) {
		SimpleStrolchElementListener listener = new SimpleStrolchElementListener();
		try (InputStream binaryStream = sqlxml.getBinaryStream()) {
			SAXParser parser = SAXParserFactory.newInstance().newSAXParser();
			parser.parse(binaryStream, new XmlModelDefaultHandler(listener));
		} catch (SQLException | IOException | SAXException | ParserConfigurationException e) {
			throw new StrolchPersistenceException(MessageFormat.format(
					"Failed to extract Resource from sqlxml value for {0} / {1}", id, type));
		}

		if (listener.getResources().size() == 0)
			throw new StrolchPersistenceException(MessageFormat.format(
					"No Resource parsed from sqlxml value for {0} / {1}", id, type));
		if (listener.getResources().size() > 1)
			throw new StrolchPersistenceException(MessageFormat.format(
					"Multiple Resources parsed from sqlxml value for {0} / {1}", id, type));

		return listener.getResources().get(0);
	}

	protected SQLXML createSqlXml(Resource res, PreparedStatement preparedStatement) throws SQLException, SAXException {
		SQLXML sqlxml = this.tx.getConnection().createSQLXML();
		SAXResult saxResult = sqlxml.setResult(SAXResult.class);
		ContentHandler contentHandler = saxResult.getHandler();
		contentHandler.startDocument();
		new ResourceToSaxVisitor(contentHandler).visit(res);
		contentHandler.endDocument();
		return sqlxml;
	}

	@Override
	protected void internalSave(final Resource res) {
		String sql = "insert into " + getTableName() + " (id, name, type, asxml) values (?, ?, ?, ?)";
		try (PreparedStatement preparedStatement = PostgreSqlResourceDao.this.tx.getConnection().prepareStatement(sql)) {
			preparedStatement.setString(1, res.getId());
			preparedStatement.setString(2, res.getName());
			preparedStatement.setString(3, res.getType());

			SQLXML sqlxml = createSqlXml(res, preparedStatement);
			preparedStatement.setSQLXML(4, sqlxml);
			try {
				preparedStatement.execute();
			} finally {
				sqlxml.free();
			}

		} catch (SQLException | SAXException e) {
			throw new StrolchPersistenceException(MessageFormat.format("Failed to insert Resource {0} due to {1}",
					res.getLocator(), e.getLocalizedMessage()), e);
		}
	}

	@Override
	protected void internalUpdate(final Resource resource) {
		String sql = "update " + getTableName() + " set name = ?, type = ?, asxml = ? where id = ? ";
		try (PreparedStatement preparedStatement = PostgreSqlResourceDao.this.tx.getConnection().prepareStatement(sql)) {

			preparedStatement.setString(1, resource.getName());
			preparedStatement.setString(2, resource.getType());
			preparedStatement.setString(4, resource.getId());

			SQLXML sqlxml = createSqlXml(resource, preparedStatement);
			preparedStatement.setSQLXML(3, sqlxml);
			try {
				preparedStatement.execute();
			} finally {
				sqlxml.free();
			}

		} catch (SQLException | SAXException e) {
			throw new StrolchPersistenceException(MessageFormat.format("Failed to update Resource {0} due to {1}",
					resource.getLocator(), e.getLocalizedMessage()), e);
		}
	}

	@Override
	public List<Resource> doQuery(ResourceQuery query) {
		// TODO Auto-generated method stub
		return null;
	}
}
