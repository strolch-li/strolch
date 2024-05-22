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

import li.strolch.privilege.model.Group;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

import java.text.MessageFormat;
import java.util.*;

import static li.strolch.privilege.helper.XmlConstants.*;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class PrivilegeGroupsSaxReader extends DefaultHandler {

	protected static final Logger logger = LoggerFactory.getLogger(PrivilegeGroupsSaxReader.class);

	private final Deque<ElementParser> buildersStack = new ArrayDeque<>();

	private final Map<String, Group> groups;

	public PrivilegeGroupsSaxReader() {
		this.groups = new HashMap<>();
	}

	/**
	 * @return the users
	 */
	public Map<String, Group> getGroups() {
		return this.groups;
	}

	@Override
	public void startElement(String uri, String localName, String qName, Attributes attributes) throws SAXException {
		if (qName.equals(GROUP)) {
			if (this.buildersStack.stream().anyMatch(e -> e.getClass().equals(GroupParser.class)))
				throw new IllegalArgumentException("Previous Group not closed!");
			this.buildersStack.push(new GroupParser());
		} else if (qName.equals(PROPERTIES)) {
			if (this.buildersStack.stream().anyMatch(e -> e.getClass().equals(PropertyParser.class)))
				throw new IllegalArgumentException("Previous Properties not closed!");
			this.buildersStack.push(new PropertyParser());
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
		if (qName.equals(GROUP)) {
			elementParser = this.buildersStack.pop();
		} else if (qName.equals(PROPERTIES)) {
			elementParser = this.buildersStack.pop();
		}

		if (!this.buildersStack.isEmpty() && elementParser != null)
			this.buildersStack.peek().notifyChild(elementParser);
	}

	//	<User userId="1" username="admin" password="8c6976e5b5410415bde908bd4dee15dfb167a9c873fc4bb8a81f6f2ab448a918">
	//	  <Firstname>Application</Firstname>
	//	  <Lastname>Administrator</Lastname>
	//	  <State>ENABLED</State>
	//	  <Locale>en-GB</Locale>
	//	  <Roles>
	//	    <Role>PrivilegeAdmin</Role>
	//	    <Role>AppUser</Role>
	//	  </Roles>
	//	  <Properties>
	//	    <Property name="organization" value="eitchnet.ch" />
	//	    <Property name="organizationalUnit" value="Development" />
	//	  </Properties>
	//    <History>
	//      <FirstLogin>2021-02-19T15:32:09.592+01:00</FirstLogin>
	//      <LastLogin>2021-02-19T15:32:09.592+01:00</LastLogin>
	//      <LastPasswordChange>2021-02-19T15:32:09.592+01:00</LastPasswordChange>
	//    </History>
	//	</User>

	public class GroupParser extends ElementParserAdapter {

		StringBuilder text;

		String name;
		final Set<String> roles;
		Map<String, String> parameters;

		public GroupParser() {
			this.roles = new HashSet<>();
			this.parameters = new HashMap<>();
		}

		@Override
		public void startElement(String uri, String localName, String qName, Attributes attributes) {

			this.text = new StringBuilder();

			if (qName.equals(GROUP)) {
				this.name = attributes.getValue(ATTR_NAME).trim();
			}
		}

		@Override
		public void characters(char[] ch, int start, int length) {
			this.text.append(ch, start, length);
		}

		@Override
		public void endElement(String uri, String localName, String qName) {

			switch (qName) {
				case ROLE -> this.roles.add(getText());
				case GROUP -> {

					Group group = new Group(this.name, this.roles, this.parameters);

					logger.info(MessageFormat.format("New Group: {0}", group));
					groups.put(this.name, group);
				}
				default -> {
					if (!(qName.equals(GROUPS) //
								  || qName.equals(ROLES) //
								  || qName.equals(PARAMETER) //
								  || qName.equals(PARAMETERS))) {
						throw new IllegalArgumentException("Unhandled tag " + qName);
					}
				}
			}
		}

		private String getText() {
			return this.text.toString().trim();
		}

		@Override
		public void notifyChild(ElementParser child) {
			if (child instanceof PropertyParser) {
				this.parameters = ((PropertyParser) child).parameterMap;
			}
		}
	}

}
