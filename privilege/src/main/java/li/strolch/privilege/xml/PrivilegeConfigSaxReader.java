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

import java.util.ArrayDeque;
import java.util.Deque;
import java.util.HashMap;
import java.util.Map;

import li.strolch.privilege.helper.XmlConstants;
import li.strolch.privilege.model.internal.PrivilegeContainerModel;
import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class PrivilegeConfigSaxReader extends DefaultHandler {

	private final Deque<ElementParser> buildersStack = new ArrayDeque<>();
	private final PrivilegeContainerModel containerModel;

	public PrivilegeConfigSaxReader(PrivilegeContainerModel containerModel) {
		this.containerModel = containerModel;
	}

	public PrivilegeContainerModel getContainerModel() {
		return this.containerModel;
	}

	@Override
	public void startElement(String uri, String localName, String qName, Attributes attributes) throws SAXException {

		switch (qName) {
		case XML_CONTAINER -> this.buildersStack.push(new ContainerParser());
		case XML_PARAMETERS -> this.buildersStack.push(new ParametersParser());
		case XML_POLICIES -> this.buildersStack.push(new PoliciesParser());
		default -> {
			// nothing to do, probably handle on stack
		}
		}

		if (!this.buildersStack.isEmpty())
			this.buildersStack.peek().startElement(uri, localName, qName, attributes);
	}

	@Override
	public void characters(char[] ch, int start, int length) throws SAXException {
		if (!this.buildersStack.isEmpty())
			this.buildersStack.peek().characters(ch, start, length);
	}

	@Override
	public void endElement(String uri, String localName, String qName) throws SAXException {

		if (!this.buildersStack.isEmpty())
			this.buildersStack.peek().endElement(uri, localName, qName);

		ElementParser elementParser = switch (qName) {
			case XML_CONTAINER, XML_PARAMETERS, XML_POLICIES -> this.buildersStack.pop();
			default -> null;
		};

		if (!this.buildersStack.isEmpty() && elementParser != null)
			this.buildersStack.peek().notifyChild(elementParser);
	}

	public class ContainerParser extends ElementParserAdapter {

		private String currentElement;

		@Override
		public void startElement(String uri, String localName, String qName, Attributes attributes) {

			switch (qName) {
			case XML_CONTAINER -> this.currentElement = qName;
			case XML_HANDLER_ENCRYPTION -> {
				this.currentElement = qName;
				String className = attributes.getValue(XML_ATTR_CLASS).trim();
				getContainerModel().setEncryptionHandlerClassName(className);
			}
			case XML_HANDLER_PASSWORD_STRENGTH -> {
				this.currentElement = qName;
				String className = attributes.getValue(XML_ATTR_CLASS).trim();
				getContainerModel().setPasswordStrengthHandlerClassName(className);
			}
			case XML_HANDLER_PERSISTENCE -> {
				this.currentElement = qName;
				String className = attributes.getValue(XML_ATTR_CLASS).trim();
				getContainerModel().setPersistenceHandlerClassName(className);
			}
			case XML_HANDLER_USER_CHALLENGE -> {
				this.currentElement = qName;
				String className = attributes.getValue(XML_ATTR_CLASS).trim();
				getContainerModel().setUserChallengeHandlerClassName(className);
			}
			case XML_HANDLER_SSO -> {
				this.currentElement = qName;
				String className = attributes.getValue(XML_ATTR_CLASS).trim();
				getContainerModel().setSsoHandlerClassName(className);
			}
			case XML_HANDLER_PRIVILEGE -> {
				this.currentElement = qName;
				String className = attributes.getValue(XML_ATTR_CLASS).trim();
				getContainerModel().setPrivilegeHandlerClassName(className);
			}
			default -> throw new IllegalStateException("Unexpected value: " + qName);
			}
		}

		@Override
		public void notifyChild(ElementParser child) {
			if (!(child instanceof ParametersParser parametersChild))
				return;

			switch (this.currentElement) {
			case XML_CONTAINER -> getContainerModel().setParameterMap(parametersChild.getParameterMap());
			case XML_HANDLER_ENCRYPTION ->
					getContainerModel().setEncryptionHandlerParameterMap(parametersChild.getParameterMap());
			case XML_HANDLER_PASSWORD_STRENGTH ->
					getContainerModel().setPasswordStrengthHandlerParameterMap(parametersChild.getParameterMap());
			case XML_HANDLER_PERSISTENCE ->
					getContainerModel().setPersistenceHandlerParameterMap(parametersChild.getParameterMap());
			case XML_HANDLER_USER_CHALLENGE ->
					getContainerModel().setUserChallengeHandlerParameterMap(parametersChild.getParameterMap());
			case XML_HANDLER_SSO -> getContainerModel().setSsoHandlerParameterMap(parametersChild.getParameterMap());
			case XML_HANDLER_PRIVILEGE ->
					getContainerModel().setPrivilegeHandlerParameterMap(parametersChild.getParameterMap());
			default -> throw new IllegalStateException("Unexpected value: " + this.currentElement);
			}
		}
	}

	static class ParametersParser extends ElementParserAdapter {

		//	      <Parameter name="autoPersistOnPasswordChange" value="true" />

		private final Map<String, String> parameterMap = new HashMap<>();

		@Override
		public void startElement(String uri, String localName, String qName, Attributes attributes) {
			if (qName.equals(XML_PARAMETER)) {
				String key = attributes.getValue(XML_ATTR_NAME).trim();
				String value = attributes.getValue(XML_ATTR_VALUE).trim();
				this.parameterMap.put(key, value);
			}
		}

		/**
		 * @return the parameterMap
		 */
		public Map<String, String> getParameterMap() {
			return this.parameterMap;
		}
	}

	class PoliciesParser extends ElementParserAdapter {

		//	    <Policy name="DefaultPrivilege" class="li.strolch.privilege.policy.DefaultPrivilege" />

		@Override
		public void startElement(String uri, String localName, String qName, Attributes attributes) {
			if (qName.equals(XML_POLICY)) {
				String policyName = attributes.getValue(XML_ATTR_NAME).trim();
				String policyClassName = attributes.getValue(XML_ATTR_CLASS).trim();

				getContainerModel().addPolicy(policyName, policyClassName);
			}
		}
	}
}