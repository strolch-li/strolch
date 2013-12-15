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
package ch.eitchnet.privilege.xml;

import java.io.File;
import java.util.Map.Entry;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

import ch.eitchnet.privilege.helper.XmlConstants;
import ch.eitchnet.privilege.model.internal.PrivilegeContainerModel;
import ch.eitchnet.privilege.policy.PrivilegePolicy;
import ch.eitchnet.utils.helper.XmlHelper;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
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
		Element rootElement = doc.createElement(XmlConstants.XML_ROOT_PRIVILEGE);
		doc.appendChild(rootElement);

		Element containerElement = doc.createElement(XmlConstants.XML_CONTAINER);
		rootElement.appendChild(containerElement);

		Element parameterElement;
		Element parametersElement;

		// Parameters
		parametersElement = doc.createElement(XmlConstants.XML_PARAMETERS);
		containerElement.appendChild(parametersElement);
		for (Entry<String, String> entry : this.containerModel.getParameterMap().entrySet()) {
			parameterElement = doc.createElement(XmlConstants.XML_PARAMETER);
			parameterElement.setAttribute(XmlConstants.XML_ATTR_NAME, entry.getKey());
			parameterElement.setAttribute(XmlConstants.XML_ATTR_VALUE, entry.getValue());
			parametersElement.appendChild(parameterElement);
		}

		// create EncryptionHandler
		Element encryptionHandlerElem = doc.createElement(XmlConstants.XML_HANDLER_ENCRYPTION);
		containerElement.appendChild(encryptionHandlerElem);
		encryptionHandlerElem.setAttribute(XmlConstants.XML_ATTR_CLASS,
				this.containerModel.getEncryptionHandlerClassName());
		// Parameters
		parametersElement = doc.createElement(XmlConstants.XML_PARAMETERS);
		encryptionHandlerElem.appendChild(parametersElement);
		for (Entry<String, String> entry : this.containerModel.getEncryptionHandlerParameterMap().entrySet()) {
			parameterElement = doc.createElement(XmlConstants.XML_PARAMETER);
			parameterElement.setAttribute(XmlConstants.XML_ATTR_NAME, entry.getKey());
			parameterElement.setAttribute(XmlConstants.XML_ATTR_VALUE, entry.getValue());
			parametersElement.appendChild(parameterElement);
		}

		// create PersistenceHandler
		Element persistenceHandlerElem = doc.createElement(XmlConstants.XML_HANDLER_PERSISTENCE);
		containerElement.appendChild(persistenceHandlerElem);
		persistenceHandlerElem.setAttribute(XmlConstants.XML_ATTR_CLASS,
				this.containerModel.getPersistenceHandlerClassName());
		// Parameters
		parametersElement = doc.createElement(XmlConstants.XML_PARAMETERS);
		persistenceHandlerElem.appendChild(parametersElement);
		for (Entry<String, String> entry : this.containerModel.getPersistenceHandlerParameterMap().entrySet()) {
			parameterElement = doc.createElement(XmlConstants.XML_PARAMETER);
			parameterElement.setAttribute(XmlConstants.XML_ATTR_NAME, entry.getKey());
			parameterElement.setAttribute(XmlConstants.XML_ATTR_VALUE, entry.getValue());
			parametersElement.appendChild(parameterElement);
		}

		// Policies
		Element policiesElem = doc.createElement(XmlConstants.XML_POLICIES);
		rootElement.appendChild(policiesElem);
		for (Entry<String, Class<PrivilegePolicy>> entry : this.containerModel.getPolicies().entrySet()) {
			Element policyElem = doc.createElement(XmlConstants.XML_POLICY);
			policyElem.setAttribute(XmlConstants.XML_ATTR_NAME, entry.getKey());
			policyElem.setAttribute(XmlConstants.XML_ATTR_CLASS, entry.getValue().getName());
			policiesElem.appendChild(policyElem);
		}

		// write the container file to disk
		XmlHelper.writeDocument(doc, this.configFile);
	}
}
