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
import li.strolch.privilege.model.Privilege;
import li.strolch.privilege.model.internal.PersonalAccessToken;
import li.strolch.utils.iso8601.ISO8601;

import javax.xml.stream.XMLStreamException;
import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

import static java.util.Comparator.comparing;
import static li.strolch.privilege.helper.XmlConstants.*;
import static li.strolch.privilege.helper.XmlHelper.openXmlStreamWriterDocument;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class PrivilegeTokensSaxWriter {

	private final List<PersonalAccessToken> tokens;
	private final File modelFile;

	public PrivilegeTokensSaxWriter(List<PersonalAccessToken> tokens, File modelFile) {
		this.tokens = tokens;
		this.modelFile = modelFile;
	}

	public void write() throws IOException, XMLStreamException {

		try (Writer ioWriter = new OutputStreamWriter(new FileOutputStream(this.modelFile), StandardCharsets.UTF_8)) {

			IndentingXMLStreamWriter xmlWriter = openXmlStreamWriterDocument(ioWriter);
			xmlWriter.writeStartElement(TOKENS);

			List<PersonalAccessToken> tokens = new ArrayList<>(this.tokens);
			tokens.sort(comparing((PersonalAccessToken t) -> t.username().toLowerCase(Locale.ROOT)).thenComparing(
					PersonalAccessToken::tokenId));
			for (PersonalAccessToken token : tokens) {

				// start the role element
				xmlWriter.writeStartElement(TOKEN);
				xmlWriter.writeAttribute(ATTR_USERNAME, token.username());
				xmlWriter.writeAttribute(ATTR_TOKEN_ID, token.tokenId());
				xmlWriter.writeAttribute(ATTR_NAME, token.name());
				xmlWriter.writeAttribute(ATTR_TOKEN, token.passwordCrypt().buildPasswordString());
				xmlWriter.writeAttribute(ATTR_VALID_FROM, ISO8601.toString(token.validFrom()));
				xmlWriter.writeAttribute(ATTR_VALID_TO, ISO8601.toString(token.validTo()));
				if (token.lastUsed() != null)
					xmlWriter.writeAttribute(ATTR_LAST_USED, ISO8601.toString(token.lastUsed()));

				List<Privilege> privileges = new ArrayList<>(token.privileges().values());
				PrivilegeRolesSaxWriter.writePrivileges(privileges, xmlWriter);

				xmlWriter.writeEndElement();
			}

			// and now end
			xmlWriter.writeEndDocument();
			xmlWriter.flush();
		}
	}
}
