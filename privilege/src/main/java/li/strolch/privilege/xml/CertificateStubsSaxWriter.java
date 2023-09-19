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

import javanet.staxutils.IndentingXMLStreamWriter;
import li.strolch.privilege.model.Certificate;
import li.strolch.utils.iso8601.ISO8601;

import javax.xml.stream.XMLStreamException;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;

import static java.util.Comparator.comparing;
import static li.strolch.privilege.helper.XmlConstants.*;
import static li.strolch.privilege.helper.XmlHelper.openXmlStreamWriterDocument;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class CertificateStubsSaxWriter {

	private final List<Certificate> certificates;
	private final OutputStream outputStream;

	public CertificateStubsSaxWriter(List<Certificate> certificates, OutputStream outputStream) {
		this.certificates = certificates;
		this.outputStream = outputStream;
	}

	public void write() throws IOException, XMLStreamException {

		Writer ioWriter = new OutputStreamWriter(this.outputStream, StandardCharsets.UTF_8);

		IndentingXMLStreamWriter xmlWriter = openXmlStreamWriterDocument(ioWriter);
		xmlWriter.writeStartElement(XML_ROOT_CERTIFICATES);

		List<Certificate> certificates = new ArrayList<>(this.certificates);
		certificates.sort(comparing(Certificate::getSessionId));
		for (Certificate cert : certificates) {

			// create the certificate element
			xmlWriter.writeStartElement(XML_CERTIFICATE);

			// sessionId;
			xmlWriter.writeAttribute(XML_ATTR_SESSION_ID, cert.getSessionId());

			// usage;
			xmlWriter.writeAttribute(XML_ATTR_USAGE, cert.getUsage().name());

			// username;
			xmlWriter.writeAttribute(XML_ATTR_USERNAME, cert.getUsername());

			// authToken;
			xmlWriter.writeAttribute(XML_ATTR_AUTH_TOKEN, cert.getAuthToken());

			// source;
			xmlWriter.writeAttribute(XML_ATTR_SOURCE, cert.getSource());

			// locale;
			xmlWriter.writeAttribute(XML_ATTR_LOCALE, cert.getLocale().toLanguageTag());

			// loginTime;
			xmlWriter.writeAttribute(XML_ATTR_LOGIN_TIME, ISO8601.toString(cert.getLoginTime()));

			// lastAccess;
			xmlWriter.writeAttribute(XML_ATTR_LAST_ACCESS, ISO8601.toString(cert.getLastAccess()));

			// keepAlive;
			xmlWriter.writeAttribute(XML_ATTR_KEEP_ALIVE, String.valueOf(cert.isKeepAlive()));
		}

		// and now end
		xmlWriter.writeEndDocument();
		xmlWriter.flush();
	}
}
