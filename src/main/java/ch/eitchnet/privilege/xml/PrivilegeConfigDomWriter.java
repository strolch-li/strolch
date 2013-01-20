/*
 * Copyright (c) 2012, Robert von Burg
 *
 * All rights reserved.
 *
 * This file is part of the Privilege.
 *
 *  Privilege is free software: you can redistribute 
 *  it and/or modify it under the terms of the GNU General Public License as 
 *  published by the Free Software Foundation, either version 3 of the License, 
 *  or (at your option) any later version.
 *
 *  Privilege is distributed in the hope that it will 
 *  be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with Privilege.  If not, see 
 *  <http://www.gnu.org/licenses/>.
 */
package ch.eitchnet.privilege.xml;

import java.io.File;
import java.util.Map.Entry;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

import ch.eitchnet.privilege.helper.XmlConstants;
import ch.eitchnet.privilege.helper.XmlHelper;
import ch.eitchnet.privilege.model.internal.PrivilegeContainerModel;
import ch.eitchnet.privilege.policy.PrivilegePolicy;

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
