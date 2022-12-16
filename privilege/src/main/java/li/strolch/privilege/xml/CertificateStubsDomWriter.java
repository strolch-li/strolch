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

import static java.util.Comparator.comparing;
import static li.strolch.privilege.helper.XmlConstants.*;

import java.io.OutputStream;
import java.util.List;

import li.strolch.privilege.model.Certificate;
import li.strolch.utils.helper.XmlHelper;
import li.strolch.utils.iso8601.ISO8601;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class CertificateStubsDomWriter {

	private final List<Certificate> certificates;
	private final OutputStream outputStream;

	public CertificateStubsDomWriter(List<Certificate> certificates, OutputStream outputStream) {
		this.certificates = certificates;
		this.outputStream = outputStream;
	}

	public void write() {

		// create document root
		Document doc = XmlHelper.createDocument();
		Element rootElement = doc.createElement(XML_ROOT_CERTIFICATES);
		doc.appendChild(rootElement);

		this.certificates.stream().sorted(comparing(Certificate::getSessionId)).forEach(cert -> {

			// create the certificate element
			Element certElement = doc.createElement(XML_CERTIFICATE);
			rootElement.appendChild(certElement);

			// sessionId;
			certElement.setAttribute(XML_ATTR_SESSION_ID, cert.getSessionId());

			// usage;
			certElement.setAttribute(XML_ATTR_USAGE, cert.getUsage().name());

			// username;
			certElement.setAttribute(XML_ATTR_USERNAME, cert.getUsername());

			// authToken;
			certElement.setAttribute(XML_ATTR_AUTH_TOKEN, cert.getAuthToken());

			// source;
			certElement.setAttribute(XML_ATTR_SOURCE, cert.getSource());

			// locale;
			certElement.setAttribute(XML_ATTR_LOCALE, cert.getLocale().toLanguageTag());

			// loginTime;
			certElement.setAttribute(XML_ATTR_LOGIN_TIME, ISO8601.toString(cert.getLoginTime()));

			// lastAccess;
			certElement.setAttribute(XML_ATTR_LAST_ACCESS, ISO8601.toString(cert.getLastAccess()));

			// keepAlive;
			certElement.setAttribute(XML_ATTR_KEEP_ALIVE, String.valueOf(cert.isKeepAlive()));
		});

		// write the container file to disk
		XmlHelper.writeDocument(doc, this.outputStream);
	}
}
