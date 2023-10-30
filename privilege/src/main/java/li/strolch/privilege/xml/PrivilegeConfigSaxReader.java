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

import li.strolch.privilege.model.internal.PrivilegeContainerModel;
import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

import java.util.ArrayDeque;
import java.util.Deque;
import java.util.HashMap;
import java.util.Map;

import static li.strolch.privilege.helper.XmlConstants.*;

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
			case CONTAINER -> this.buildersStack.push(new ContainerParser());
			case PARAMETERS -> this.buildersStack.push(new ParametersParser());
			case POLICIES -> this.buildersStack.push(new PoliciesParser());
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
			case CONTAINER, PARAMETERS, POLICIES -> this.buildersStack.pop();
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
				case CONTAINER -> this.currentElement = qName;
				case HANDLER_PRIVILEGE -> {
					this.currentElement = qName;
					String className = attributes.getValue(ATTR_CLASS).trim();
					getContainerModel().setPrivilegeHandlerClassName(className);
				}
				case HANDLER_ENCRYPTION -> {
					this.currentElement = qName;
					String className = attributes.getValue(ATTR_CLASS).trim();
					getContainerModel().setEncryptionHandlerClassName(className);
				}
				case HANDLER_PASSWORD_STRENGTH -> {
					this.currentElement = qName;
					String className = attributes.getValue(ATTR_CLASS).trim();
					getContainerModel().setPasswordStrengthHandlerClassName(className);
				}
				case HANDLER_PERSISTENCE -> {
					this.currentElement = qName;
					String className = attributes.getValue(ATTR_CLASS).trim();
					getContainerModel().setPersistenceHandlerClassName(className);
				}
				case HANDLER_USER_CHALLENGE -> {
					this.currentElement = qName;
					String className = attributes.getValue(ATTR_CLASS).trim();
					getContainerModel().setUserChallengeHandlerClassName(className);
				}
				case HANDLER_SSO -> {
					this.currentElement = qName;
					String className = attributes.getValue(ATTR_CLASS).trim();
					getContainerModel().setSsoHandlerClassName(className);
				}
				default -> throw new IllegalStateException("Unexpected value: " + qName);
			}
		}

		@Override
		public void notifyChild(ElementParser child) {
			if (!(child instanceof ParametersParser parametersChild))
				return;

			final Map<String, String> params = parametersChild.getParameterMap();
			switch (this.currentElement) {
				case CONTAINER -> getContainerModel().setParameterMap(params);
				case HANDLER_PRIVILEGE -> getContainerModel().setPrivilegeHandlerParameterMap(params);
				case HANDLER_ENCRYPTION -> getContainerModel().setEncryptionHandlerParameterMap(params);
				case HANDLER_PASSWORD_STRENGTH ->
						getContainerModel().setPasswordStrengthHandlerParameterMap(params);
				case HANDLER_PERSISTENCE -> getContainerModel().setPersistenceHandlerParameterMap(params);
				case HANDLER_USER_CHALLENGE -> getContainerModel().setUserChallengeHandlerParameterMap(params);
				case HANDLER_SSO -> getContainerModel().setSsoHandlerParameterMap(params);
				default -> throw new IllegalStateException("Unexpected value: " + this.currentElement);
			}
		}
	}

	static class ParametersParser extends ElementParserAdapter {

		//	      <Parameter name="autoPersistOnPasswordChange" value="true" />

		private final Map<String, String> parameterMap = new HashMap<>();

		@Override
		public void startElement(String uri, String localName, String qName, Attributes attributes) {
			if (qName.equals(PARAMETER)) {
				String key = attributes.getValue(ATTR_NAME).trim();
				String value = attributes.getValue(ATTR_VALUE).trim();
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
			if (qName.equals(POLICY)) {
				String policyName = attributes.getValue(ATTR_NAME).trim();
				String policyClassName = attributes.getValue(ATTR_CLASS).trim();

				getContainerModel().addPolicy(policyName, policyClassName);
			}
		}
	}
}