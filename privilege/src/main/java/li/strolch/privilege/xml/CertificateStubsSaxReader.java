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

import li.strolch.privilege.base.PrivilegeException;
import li.strolch.privilege.model.Usage;
import li.strolch.utils.dbc.DBC;
import li.strolch.utils.helper.XmlHelper;
import li.strolch.utils.iso8601.ISO8601;
import org.xml.sax.Attributes;
import org.xml.sax.helpers.DefaultHandler;

import java.io.InputStream;
import java.time.ZonedDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

import static li.strolch.privilege.handler.DefaultPrivilegeHandler.SOURCE_UNKNOWN;
import static li.strolch.privilege.helper.XmlConstants.*;
import static li.strolch.utils.helper.StringHelper.isEmpty;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class CertificateStubsSaxReader extends DefaultHandler {

	private final InputStream inputStream;
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
	public void startElement(String uri, String localName, String qName, Attributes attributes) {

		switch (qName) {
			case ROOT_CERTIFICATES -> {
			}
			case CERTIFICATE -> {
				CertificateStub stub = new CertificateStub();
				stub.usage = Usage.valueOf(attributes.getValue(ATTR_USAGE).trim());
				stub.sessionId = attributes.getValue(ATTR_SESSION_ID).trim();
				stub.username = attributes.getValue(ATTR_USERNAME).trim();
				stub.authToken = attributes.getValue(ATTR_AUTH_TOKEN).trim();
				stub.source = attributes.getValue(ATTR_SOURCE).trim();
				stub.locale = Locale.forLanguageTag(attributes.getValue(ATTR_LOCALE).trim());
				stub.loginTime = ISO8601.parseToZdt(attributes.getValue(ATTR_LOGIN_TIME).trim());
				stub.lastAccess = ISO8601.parseToZdt(attributes.getValue(ATTR_LAST_ACCESS).trim());
				stub.keepAlive = Boolean.parseBoolean(attributes.getValue(ATTR_KEEP_ALIVE).trim());
				DBC.INTERIM.assertNotEmpty("sessionId missing on sessions data!", stub.sessionId);
				DBC.INTERIM.assertNotEmpty("username missing on sessions data!", stub.username);
				DBC.INTERIM.assertNotEmpty("authToken missing on sessions data!", stub.authToken);
				if (isEmpty(stub.source))
					stub.source = SOURCE_UNKNOWN;
				this.stubs.add(stub);
			}
			default -> throw new PrivilegeException("Unhandled tag " + qName);
		}
	}

	public static class CertificateStub {
		private Usage usage;
		private String sessionId;
		private String username;
		private String authToken;
		private String source;
		private Locale locale;
		private ZonedDateTime loginTime;
		private ZonedDateTime lastAccess;
		private boolean keepAlive;

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

		public ZonedDateTime getLoginTime() {
			return loginTime;
		}

		public ZonedDateTime getLastAccess() {
			return lastAccess;
		}

		public boolean isKeepAlive() {
			return this.keepAlive;
		}
	}
}
