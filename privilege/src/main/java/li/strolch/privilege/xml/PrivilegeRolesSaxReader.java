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

import java.text.MessageFormat;
import java.util.*;

import li.strolch.privilege.helper.XmlConstants;
import li.strolch.privilege.model.IPrivilege;
import li.strolch.privilege.model.internal.PrivilegeImpl;
import li.strolch.privilege.model.internal.Role;
import li.strolch.utils.helper.StringHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class PrivilegeRolesSaxReader extends DefaultHandler {

	protected static final Logger logger = LoggerFactory.getLogger(PrivilegeRolesSaxReader.class);

	private final Deque<ElementParser> buildersStack = new ArrayDeque<>();

	private final List<Role> roles;

	public PrivilegeRolesSaxReader() {
		this.roles = new ArrayList<>();
	}

	public List<Role> getRoles() {
		return this.roles;
	}

	@Override
	public void startElement(String uri, String localName, String qName, Attributes attributes) throws SAXException {

		if (qName.equals(XmlConstants.XML_ROLE)) {
			this.buildersStack.push(new RoleParser());
		} else if (qName.equals(XmlConstants.XML_PROPERTIES)) {
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
		if (qName.equals(XmlConstants.XML_ROLE)) {
			elementParser = this.buildersStack.pop();
		} else if (qName.equals(XmlConstants.XML_PROPERTIES)) {
			elementParser = this.buildersStack.pop();
		}

		if (!this.buildersStack.isEmpty() && elementParser != null)
			this.buildersStack.peek().notifyChild(elementParser);
	}

//	<Role name="AppUser">
//	  <Privilege name="li.strolch.privilege.test.model.TestRestrictable">
//	    <AllAllowed>true</AllAllowed>
//	  </Privilege>
//	</Role>
//	<Role name="system_admin_privileges">
//	  <Privilege name="li.strolch.privilege.test.model.TestSystemUserAction">
//	    <AllAllowed>true</AllAllowed>
//	  </Privilege>
//	  <Privilege name="li.strolch.privilege.test.model.TestSystemRestrictable">
//	    <AllAllowed>true</AllAllowed>
//	  </Privilege>
//	</Role>

	public class RoleParser extends ElementParserAdapter {

		private StringBuilder text;

		private String roleName;
		private String privilegeName;
		private String privilegePolicy;
		private boolean allAllowed;
		private Set<String> denyList;
		private Set<String> allowList;

		private Map<String, IPrivilege> privileges;

		public RoleParser() {
			init();
		}

		private void init() {
			this.privileges = new HashMap<>();

			this.text = null;

			this.roleName = null;
			this.privilegeName = null;
			this.privilegePolicy = null;
			this.allAllowed = false;
			this.denyList = new HashSet<>();
			this.allowList = new HashSet<>();
		}

		@Override
		public void startElement(String uri, String localName, String qName, Attributes attributes) {

			this.text = new StringBuilder();

			switch (qName) {
			case XmlConstants.XML_ROLE:
				this.roleName = attributes.getValue(XmlConstants.XML_ATTR_NAME).trim();
				break;
			case XmlConstants.XML_PRIVILEGE:
				this.privilegeName = attributes.getValue(XmlConstants.XML_ATTR_NAME).trim();
				this.privilegePolicy = attributes.getValue(XmlConstants.XML_ATTR_POLICY).trim();
				break;
			case XmlConstants.XML_ALLOW:
			case XmlConstants.XML_DENY:
			case XmlConstants.XML_ALL_ALLOWED:
				// no-op
				break;
			default:
				throw new IllegalArgumentException("Unhandled tag " + qName);
			}
		}

		@Override
		public void characters(char[] ch, int start, int length) {
			if (this.text != null)
				this.text.append(ch, start, length);
		}

		@Override
		public void endElement(String uri, String localName, String qName) {

			switch (qName) {
			case XmlConstants.XML_ALL_ALLOWED ->
					this.allAllowed = StringHelper.parseBoolean(this.text.toString().trim());
			case XmlConstants.XML_ALLOW -> this.allowList.add(this.text.toString().trim());
			case XmlConstants.XML_DENY -> this.denyList.add(this.text.toString().trim());
			case XmlConstants.XML_PRIVILEGE -> {
				IPrivilege privilege = new PrivilegeImpl(this.privilegeName, this.privilegePolicy, this.allAllowed,
						this.denyList, this.allowList);
				this.privileges.put(this.privilegeName, privilege);
				this.privilegeName = null;
				this.privilegePolicy = null;
				this.allAllowed = false;
				this.denyList = new HashSet<>();
				this.allowList = new HashSet<>();
			}
			case XmlConstants.XML_ROLE -> {
				Role role = new Role(this.roleName, this.privileges);
				getRoles().add(role);
				logger.info(MessageFormat.format("New Role: {0}", role));
				init();
			}
			}
		}
	}

	static class PropertyParser extends ElementParserAdapter {

//	      <Property name="organizationalUnit" value="Development" />

		public final Map<String, String> parameterMap = new HashMap<>();

		@Override
		public void startElement(String uri, String localName, String qName, Attributes attributes) {
			if (qName.equals(XmlConstants.XML_PROPERTY)) {
				String key = attributes.getValue(XmlConstants.XML_ATTR_NAME).trim();
				String value = attributes.getValue(XmlConstants.XML_ATTR_VALUE).trim();
				this.parameterMap.put(key, value);
			} else if (qName.equals(XmlConstants.XML_PROPERTIES)) {
				// NO-OP
			} else {
				throw new IllegalArgumentException("Unhandled tag " + qName);
			}
		}

		public Map<String, String> getParameterMap() {
			return this.parameterMap;
		}
	}
}
