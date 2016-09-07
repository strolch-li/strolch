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

import java.io.OutputStream;
import java.util.List;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

import li.strolch.privilege.helper.XmlConstants;
import li.strolch.privilege.model.Certificate;
import li.strolch.utils.helper.XmlHelper;
import li.strolch.utils.iso8601.ISO8601FormatFactory;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class CertificateStubsDomWriter {

	private List<Certificate> certificates;
	private OutputStream outputStream;

	public CertificateStubsDomWriter(List<Certificate> certificates, OutputStream outputStream) {
		this.certificates = certificates;
		this.outputStream = outputStream;
	}

	public void write() {

		// create document root
		Document doc = XmlHelper.createDocument();
		Element rootElement = doc.createElement(XmlConstants.XML_ROOT_CERTIFICATES);
		doc.appendChild(rootElement);

		this.certificates.stream().sorted((c1, c2) -> c1.getSessionId().compareTo(c2.getSessionId())).forEach(cert -> {

			// create the certificate element
			Element certElement = doc.createElement(XmlConstants.XML_CERTIFICATE);
			rootElement.appendChild(certElement);

			// sessionId;
			certElement.setAttribute(XmlConstants.XML_ATTR_SESSION_ID, cert.getSessionId());

			// usage;
			certElement.setAttribute(XmlConstants.XML_ATTR_USAGE, cert.getUsage().name());

			// username;
			certElement.setAttribute(XmlConstants.XML_ATTR_USERNAME, cert.getUsername());

			// authToken;
			certElement.setAttribute(XmlConstants.XML_ATTR_AUTH_TOKEN, cert.getAuthToken());

			// locale;
			certElement.setAttribute(XmlConstants.XML_ATTR_LOCALE, cert.getLocale().toString());

			// loginTime;
			certElement.setAttribute(XmlConstants.XML_ATTR_LOGIN_TIME,
					ISO8601FormatFactory.getInstance().formatDate(cert.getLoginTime()));

			// lastAccess;
			certElement.setAttribute(XmlConstants.XML_ATTR_LAST_ACCESS,
					ISO8601FormatFactory.getInstance().formatDate(cert.getLastAccess()));
		});

		// write the container file to disk
		XmlHelper.writeDocument(doc, this.outputStream);
	}
}
