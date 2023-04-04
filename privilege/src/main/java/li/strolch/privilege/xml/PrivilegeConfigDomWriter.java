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

import static li.strolch.privilege.helper.XmlConstants.*;

import java.io.File;
import java.util.Comparator;
import java.util.Map;
import java.util.Map.Entry;

import li.strolch.privilege.model.internal.PrivilegeContainerModel;
import li.strolch.utils.helper.XmlHelper;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class PrivilegeConfigDomWriter {

	private final PrivilegeContainerModel containerModel;
	private final File configFile;

	/**
	 *
	 */
	public PrivilegeConfigDomWriter(PrivilegeContainerModel containerModel, File configFile) {
		this.containerModel = containerModel;
		this.configFile = configFile;
	}

	/**
	 *
	 */
	public void write() {

		// create document root
		Document doc = XmlHelper.createDocument();
		Element rootElement = doc.createElement(XML_ROOT_PRIVILEGE);
		doc.appendChild(rootElement);

		Element containerElement = doc.createElement(XML_CONTAINER);
		rootElement.appendChild(containerElement);

		// create EncryptionHandler
		Element encryptionHandlerElem = doc.createElement(XML_HANDLER_ENCRYPTION);
		containerElement.appendChild(encryptionHandlerElem);
		encryptionHandlerElem.setAttribute(XML_ATTR_CLASS, this.containerModel.getEncryptionHandlerClassName());
		// Parameters
		fillParameterMap(doc, encryptionHandlerElem, this.containerModel.getEncryptionHandlerParameterMap());

		// create PersistenceHandler
		Element persistenceHandlerElem = doc.createElement(XML_HANDLER_PERSISTENCE);
		containerElement.appendChild(persistenceHandlerElem);
		persistenceHandlerElem.setAttribute(XML_ATTR_CLASS, this.containerModel.getPersistenceHandlerClassName());
		// Parameters
		fillParameterMap(doc, persistenceHandlerElem, this.containerModel.getPersistenceHandlerParameterMap());

		// Parameters
		fillParameterMap(doc, containerElement, this.containerModel.getParameterMap());

		// create UserChallengeHandler
		Element userChallengeHandlerElem = doc.createElement(XML_HANDLER_USER_CHALLENGE);
		containerElement.appendChild(userChallengeHandlerElem);
		userChallengeHandlerElem.setAttribute(XML_ATTR_CLASS, this.containerModel.getUserChallengeHandlerClassName());
		// Parameters
		fillParameterMap(doc, userChallengeHandlerElem, this.containerModel.getUserChallengeHandlerParameterMap());

		// create SingleSignOnHandler
		if (this.containerModel.getSsoHandlerClassName() != null) {
			Element ssoHandlerElem = doc.createElement(XML_HANDLER_SSO);
			containerElement.appendChild(ssoHandlerElem);
			ssoHandlerElem.setAttribute(XML_ATTR_CLASS, this.containerModel.getSsoHandlerClassName());
			// Parameters
			fillParameterMap(doc, ssoHandlerElem, this.containerModel.getSsoHandlerParameterMap());
		}

		// create PrivilegeHandler
		if (this.containerModel.getSsoHandlerClassName() != null) {
			Element privilegeHandlerElem = doc.createElement(XML_HANDLER_PRIVILEGE);
			containerElement.appendChild(privilegeHandlerElem);
			privilegeHandlerElem.setAttribute(XML_ATTR_CLASS, this.containerModel.getPrivilegeHandlerClassName());
			// Parameters
			fillParameterMap(doc, privilegeHandlerElem, this.containerModel.getPrivilegeHandlerParameterMap());
		}

		// Policies
		Element policiesElem = doc.createElement(XML_POLICIES);
		rootElement.appendChild(policiesElem);
		this.containerModel.getPolicies().entrySet().stream().sorted(Entry.comparingByKey())
				.forEach(entry -> {
					Element policyElem = doc.createElement(XML_POLICY);
					policyElem.setAttribute(XML_ATTR_NAME, entry.getKey());
					policyElem.setAttribute(XML_ATTR_CLASS, entry.getValue().getName());
					policiesElem.appendChild(policyElem);
				});

		// write the container file to disk
		XmlHelper.writeDocument(doc, this.configFile);
	}

	private void fillParameterMap(Document doc, Element parent, Map<String, String> parameterMap) {

		if (parameterMap != null && !parameterMap.isEmpty()) {
			Element parametersElement = doc.createElement(XML_PARAMETERS);
			for (Entry<String, String> entry : parameterMap.entrySet()) {
				Element parameterElement = doc.createElement(XML_PARAMETER);
				parameterElement.setAttribute(XML_ATTR_NAME, entry.getKey());
				parameterElement.setAttribute(XML_ATTR_VALUE, entry.getValue());
				parametersElement.appendChild(parameterElement);
			}

			parent.appendChild(parametersElement);
		}
	}
}
