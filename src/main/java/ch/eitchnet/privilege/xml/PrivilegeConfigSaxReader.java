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

import java.util.HashMap;
import java.util.Map;
import java.util.Stack;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

import ch.eitchnet.privilege.model.internal.PrivilegeContainerModel;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public class PrivilegeConfigSaxReader extends DefaultHandler {

	// private static final Logger logger = LoggerFactory.getLogger(PrivilegeConfigSaxReader.class);

	private Stack<ElementParser> buildersStack = new Stack<ElementParser>();

	private PrivilegeContainerModel containerModel;

	public PrivilegeConfigSaxReader(PrivilegeContainerModel containerModel) {
		this.containerModel = containerModel;
	}

	@Override
	public void startElement(String uri, String localName, String qName, Attributes attributes) throws SAXException {

		if (qName.equals("Container")) {
			this.buildersStack.add(new ContainerParser());
		} else if (qName.equals("Parameters")) {
			this.buildersStack.add(new ParametersParser());
		} else if (qName.equals("Policies")) {
			this.buildersStack.add(new PoliciesParser());
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

		ElementParser elementParser = null;
		if (qName.equals("Container")) {
			elementParser = this.buildersStack.pop();
		} else if (qName.equals("Parameters")) {
			elementParser = this.buildersStack.pop();
		} else if (qName.equals("Policies")) {
			elementParser = this.buildersStack.pop();
		}

		if (!this.buildersStack.isEmpty() && elementParser != null)
			this.buildersStack.peek().notifyChild(elementParser);
	}

	public class ContainerParser extends ElementParserAdapter {

//		  <Container>
//		    <Parameters>
//		      <!-- parameters for the container itself -->
//		      <Parameter name="autoPersistOnPasswordChange" value="true" />
//		    </Parameters>
//		    <EncryptionHandler class="ch.eitchnet.privilege.handler.DefaultEncryptionHandler">
//		      <Parameters>
//		        <Parameter name="hashAlgorithm" value="SHA-256" />
//		      </Parameters>
//		    </EncryptionHandler>
//		    <PersistenceHandler class="ch.eitchnet.privilege.handler.XmlPersistenceHandler">
//		      <Parameters>
//		        <Parameter name="basePath" value="./target/test" />
//		        <Parameter name="modelXmlFile" value="PrivilegeModel.xml" />
//		      </Parameters>
//		    </PersistenceHandler>
//		  </Container>

		private String currentElement;

		@Override
		public void startElement(String uri, String localName, String qName, Attributes attributes) throws SAXException {
			if (qName.equals("Container")) {
				this.currentElement = qName;
			} else if (qName.equals("EncryptionHandler")) {
				this.currentElement = qName;
				PrivilegeConfigSaxReader.this.containerModel
						.setEncryptionHandlerClassName(attributes.getValue("class"));
			} else if (qName.equals("PersistenceHandler")) {
				this.currentElement = qName;
				PrivilegeConfigSaxReader.this.containerModel.setPersistenceHandlerClassName(attributes
						.getValue("class"));
			}
		}

		@Override
		public void notifyChild(ElementParser child) {
			if (!(child instanceof ParametersParser))
				return;

			ParametersParser parametersChild = (ParametersParser) child;

			if (this.currentElement.equals("Container")) {
				PrivilegeConfigSaxReader.this.containerModel.setParameterMap(parametersChild.getParameterMap());
			} else if (this.currentElement.equals("EncryptionHandler")) {
				PrivilegeConfigSaxReader.this.containerModel.setEncryptionHandlerParameterMap(parametersChild
						.getParameterMap());
			} else if (this.currentElement.equals("PersistenceHandler")) {
				PrivilegeConfigSaxReader.this.containerModel.setPersistenceHandlerParameterMap(parametersChild
						.getParameterMap());
			}
		}
	}

	class ParametersParser extends ElementParserAdapter {

//	      <Parameter name="autoPersistOnPasswordChange" value="true" />

		private Map<String, String> parameterMap = new HashMap<String, String>();

		@Override
		public void startElement(String uri, String localName, String qName, Attributes attributes) throws SAXException {
			if (qName.equals("Parameter")) {
				String key = attributes.getValue("name");
				String value = attributes.getValue("value");
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

//	    <Policy name="DefaultPrivilege" class="ch.eitchnet.privilege.policy.DefaultPrivilege" />

		@Override
		public void startElement(String uri, String localName, String qName, Attributes attributes) throws SAXException {
			if (qName.equals("Policy")) {
				String policyName = attributes.getValue("name");
				String policyClassName = attributes.getValue("class");

				PrivilegeConfigSaxReader.this.containerModel.addPolicy(policyName, policyClassName);
			}
		}
	}
}