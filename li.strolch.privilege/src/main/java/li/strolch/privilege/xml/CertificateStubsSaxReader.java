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
package li.strolch.privilege.xml;

import static li.strolch.privilege.handler.DefaultPrivilegeHandler.SOURCE_UNKNOWN;
import static li.strolch.utils.helper.StringHelper.isEmpty;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Locale;

import li.strolch.privilege.base.PrivilegeException;
import li.strolch.privilege.helper.XmlConstants;
import li.strolch.privilege.model.Usage;
import li.strolch.utils.dbc.DBC;
import li.strolch.utils.helper.XmlHelper;
import li.strolch.utils.iso8601.ISO8601FormatFactory;
import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class CertificateStubsSaxReader extends DefaultHandler {

	private InputStream inputStream;
	private List<CertificateStub> stubs;

	public CertificateStubsSaxReader(InputStream inputStream) {
		this.inputStream = inputStream;
	}

	public List<CertificateStub> read() {
		this.stubs = new ArrayList<>();
		XmlHelper.parseDocument(this.inputStream, this);
		return stubs;
	}

	@Override
	public void startElement(String uri, String localName, String qName, Attributes attributes) throws SAXException {

		switch (qName) {
		case XmlConstants.XML_ROOT_CERTIFICATES:
			break;
		case XmlConstants.XML_CERTIFICATE:

			CertificateStub stub = new CertificateStub();
			stub.usage = Usage.valueOf(attributes.getValue(XmlConstants.XML_ATTR_USAGE));
			stub.sessionId = attributes.getValue(XmlConstants.XML_ATTR_SESSION_ID);
			stub.username = attributes.getValue(XmlConstants.XML_ATTR_USERNAME);
			stub.authToken = attributes.getValue(XmlConstants.XML_ATTR_AUTH_TOKEN);
			stub.source = attributes.getValue(XmlConstants.XML_ATTR_SOURCE);
			stub.locale = new Locale(attributes.getValue(XmlConstants.XML_ATTR_LOCALE));
			stub.loginTime = ISO8601FormatFactory.getInstance()
					.parseDate(attributes.getValue(XmlConstants.XML_ATTR_LOGIN_TIME));
			stub.lastAccess = ISO8601FormatFactory.getInstance()
					.parseDate(attributes.getValue(XmlConstants.XML_ATTR_LAST_ACCESS));

			DBC.INTERIM.assertNotEmpty("sessionId missing on sessions data!", stub.sessionId);
			DBC.INTERIM.assertNotEmpty("username missing on sessions data!", stub.username);
			DBC.INTERIM.assertNotEmpty("authToken missing on sessions data!", stub.authToken);

			if (isEmpty(stub.source))
				stub.source = SOURCE_UNKNOWN;

			this.stubs.add(stub);
			break;

		default:
			throw new PrivilegeException("Unhandled tag " + qName);
		}
	}

	public class CertificateStub {
		private Usage usage;
		private String sessionId;
		private String username;
		private String authToken;
		private String source;
		private Locale locale;
		private Date loginTime;
		private Date lastAccess;

		public Usage getUsage() {
			return this.usage;
		}

		public String getSessionId() {
			return sessionId;
		}

		public String getUsername() {
			return username;
		}

		public String getAuthToken() {
			return authToken;
		}

		public String getSource() {
			return source;
		}

		public Locale getLocale() {
			return locale;
		}

		public Date getLoginTime() {
			return loginTime;
		}

		public Date getLastAccess() {
			return lastAccess;
		}
	}
}
