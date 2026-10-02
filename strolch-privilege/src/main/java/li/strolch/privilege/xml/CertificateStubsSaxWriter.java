/*
 * Copyright (c) 2013-2025 Robert von Burg <eitch@eitchnet.ch>
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
import li.strolch.privilege.xml.CertificateStubsSaxReader.CertificateStub;
import li.strolch.utils.iso8601.ISO8601;

import javax.xml.stream.XMLStreamException;
import java.io.*;
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

	private final List<CertificateStub> certificates;
	private final OutputStream outputStream;
	private final File file;

	public CertificateStubsSaxWriter(List<CertificateStub> certificates, OutputStream outputStream) {
		this.certificates = certificates;
		this.outputStream = outputStream;
		this.file = null;
	}

	public CertificateStubsSaxWriter(List<CertificateStub> certificates, File file) {
		this.certificates = certificates;
		this.outputStream = null;
		this.file = file;
	}

	public static CertificateStubsSaxWriter ofCertificates(List<Certificate> certificates, OutputStream outputStream) {
		return new CertificateStubsSaxWriter(certificates.stream().map(CertificateStub::new).toList(), outputStream);
	}

	public static CertificateStubsSaxWriter ofCertificates(List<Certificate> certificates, File file) {
		return new CertificateStubsSaxWriter(certificates.stream().map(CertificateStub::new).toList(), file);
	}

	public void write() throws IOException, XMLStreamException {
		if (this.file != null) {
			try (OutputStream out = new FileOutputStream(this.file)) {
				writeToStream(out);
			}
		} else {
			writeToStream(this.outputStream);
		}
	}

	private void writeToStream(OutputStream outputStream) throws XMLStreamException {
		Writer ioWriter = new OutputStreamWriter(outputStream, StandardCharsets.UTF_8);

		IndentingXMLStreamWriter xmlWriter = openXmlStreamWriterDocument(ioWriter);
		xmlWriter.writeStartElement(ROOT_CERTIFICATES);

		List<CertificateStub> certificates = new ArrayList<>(this.certificates);
		certificates.sort(comparing(CertificateStub::getSessionId));
		for (CertificateStub cert : certificates) {

			if (cert.getAuthTokenCrypt().isInvalid())
				continue;

			// create the certificate element
			xmlWriter.writeEmptyElement(CERTIFICATE);

			// sessionId;
			xmlWriter.writeAttribute(ATTR_SESSION_ID, cert.getSessionId());

			// usage;
			xmlWriter.writeAttribute(ATTR_USAGE, cert.getUsage().name());

			// username;
			xmlWriter.writeAttribute(ATTR_USERNAME, cert.getUsername());

			// authToken;
			xmlWriter.writeAttribute(ATTR_AUTH_TOKEN, cert.getAuthTokenCrypt().buildPasswordString());

			// source;
			xmlWriter.writeAttribute(ATTR_SOURCE, cert.getSource());

			// locale;
			xmlWriter.writeAttribute(ATTR_LOCALE, cert.getLocale().toLanguageTag());

			// loginTime;
			xmlWriter.writeAttribute(ATTR_LOGIN_TIME, ISO8601.toString(cert.getLoginTime()));

			// lastAccess;
			xmlWriter.writeAttribute(ATTR_LAST_ACCESS, ISO8601.toString(cert.getLastAccess()));

			// keepAlive;
			xmlWriter.writeAttribute(ATTR_KEEP_ALIVE, String.valueOf(cert.isKeepAlive()));
		}

		// and now end
		xmlWriter.writeEndElement();
		xmlWriter.writeEndDocument();
		xmlWriter.flush();
	}
}
