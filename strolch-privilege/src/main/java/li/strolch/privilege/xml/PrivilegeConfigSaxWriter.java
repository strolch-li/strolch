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
import li.strolch.privilege.model.internal.PrivilegeContainerModel;

import javax.xml.stream.XMLStreamException;
import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.Map;

import static li.strolch.privilege.helper.XmlConstants.*;
import static li.strolch.privilege.helper.XmlHelper.openXmlStreamWriterDocument;
import static li.strolch.privilege.helper.XmlHelper.writeStringMapElement;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class PrivilegeConfigSaxWriter {

	private final PrivilegeContainerModel containerModel;
	private final File configFile;

	public PrivilegeConfigSaxWriter(PrivilegeContainerModel containerModel, File configFile) {
		this.containerModel = containerModel;
		this.configFile = configFile;
	}

	public void write() throws IOException, XMLStreamException {

		try (Writer ioWriter = new OutputStreamWriter(new FileOutputStream(this.configFile), StandardCharsets.UTF_8)) {

			IndentingXMLStreamWriter xmlWriter = openXmlStreamWriterDocument(ioWriter);
			xmlWriter.writeStartElement(ROOT_PRIVILEGE);

			// write container element
			xmlWriter.writeStartElement(CONTAINER);
			writeStringMapElement(xmlWriter, this.containerModel.getParameterMap(), PARAMETERS, PARAMETER);

			{
				// write PrivilegeHandler
				if (this.containerModel.getPrivilegeHandlerClassName() != null)
					writeHandler(xmlWriter, HANDLER_PRIVILEGE, this.containerModel.getPrivilegeHandlerClassName(),
							this.containerModel.getPrivilegeHandlerParameterMap());

				// write EncryptionHandler
				writeHandler(xmlWriter, HANDLER_ENCRYPTION, this.containerModel.getEncryptionHandlerClassName(),
						this.containerModel.getEncryptionHandlerParameterMap());

				// write PersistenceHandler
				writeHandler(xmlWriter, HANDLER_PERSISTENCE, this.containerModel.getPersistenceHandlerClassName(),
						this.containerModel.getPersistenceHandlerParameterMap());

				// write PasswordStrengthHandler
				if (this.containerModel.getPasswordStrengthHandlerClassName() != null)
					writeHandler(xmlWriter, HANDLER_PASSWORD_STRENGTH,
							this.containerModel.getPasswordStrengthHandlerClassName(),
							this.containerModel.getPasswordStrengthHandlerParameterMap());

				// write UserChallengeHandler
				if (this.containerModel.getUserChallengeHandlerClassName() != null)
					writeHandler(xmlWriter, HANDLER_USER_CHALLENGE,
							this.containerModel.getUserChallengeHandlerClassName(),
							this.containerModel.getUserChallengeHandlerParameterMap());

				// write SingleSignOnHandler
				if (this.containerModel.getSsoHandlerClassName() != null)
					writeHandler(xmlWriter, HANDLER_SSO, this.containerModel.getSsoHandlerClassName(),
							this.containerModel.getSsoHandlerParameterMap());
			}

			xmlWriter.writeEndElement();

			// Policies
			Map<String, String> policies = new HashMap<>();
			this.containerModel.getPolicies().forEach((key, value) -> policies.put(key, value.getName()));
			writeStringMapElement(xmlWriter, policies, POLICIES, POLICY, ATTR_CLASS);

			// and now end
			xmlWriter.writeEndDocument();
			xmlWriter.flush();
		}
	}

	private void writeHandler(IndentingXMLStreamWriter xmlWriter, String handleName, String className,
			Map<String, String> parameters) throws XMLStreamException {
		if (parameters.isEmpty())
			xmlWriter.writeEmptyElement(handleName);
		else
			xmlWriter.writeStartElement(handleName);
		xmlWriter.writeAttribute(ATTR_CLASS, className);

		writeStringMapElement(xmlWriter, parameters, PARAMETERS, PARAMETER);

		if (!parameters.isEmpty())
			xmlWriter.writeEndElement();
	}
}
