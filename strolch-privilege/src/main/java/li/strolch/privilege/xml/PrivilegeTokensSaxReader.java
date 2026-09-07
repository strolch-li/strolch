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

import li.strolch.privilege.model.Privilege;
import li.strolch.privilege.model.internal.PersonalAccessToken;
import li.strolch.privilege.model.internal.PasswordCrypt;
import li.strolch.utils.helper.StringHelper;
import li.strolch.utils.iso8601.ISO8601;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

import java.time.ZonedDateTime;
import java.util.*;
import java.util.function.Consumer;

import static li.strolch.privilege.helper.XmlConstants.*;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class PrivilegeTokensSaxReader extends DefaultHandler {

	protected static final Logger logger = LoggerFactory.getLogger(PrivilegeTokensSaxReader.class);

	private final Deque<DefaultHandler> buildersStack = new ArrayDeque<>();

	private final Map<String, PersonalAccessToken> tokens;
	private final boolean verbose;

	public PrivilegeTokensSaxReader(boolean verbose) {
		this.verbose = verbose;
		this.tokens = new HashMap<>();
	}

	public Map<String, PersonalAccessToken> getTokens() {
		return this.tokens;
	}

	@Override
	public void startElement(String uri, String localName, String qName, Attributes attributes) throws SAXException {

		if (localName.equals(TOKEN)) {
			if (this.buildersStack.stream().anyMatch(e -> e.getClass().equals(AccessTokenParser.class)))
				throw new IllegalArgumentException("Previous PersonalAccessToken not closed!");
			this.buildersStack.push(new AccessTokenParser());
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

		if (localName.equals(TOKEN)) {
			this.buildersStack.pop();
		}
	}

	//<Token username="admin" tokenId="50b31270-bc49-4940-97ec-d4aa0d1ad649"
	//       token="$PBKDF2WithHmacSHA512,10000,256$61646d696e$cb69962946617da006a2f95776d78b49e5ec7941d2bdb2d25cdb05f957f64344"
	//       validFrom="2024-02-12T08:00:00.000+01:00" validTo="3000-01-01T01:00:00.000+01:00">
	//    <Privilege name="Foo" policy="DefaultPrivilege">
	//        <Allow>allow1</Allow>
	//        <Deny>deny1</Deny>
	//    </Privilege>
	//    <Privilege name="Bar" policy="DefaultPrivilege">
	//        <Allow>allow2</Allow>
	//        <Deny>deny2</Deny>
	//    </Privilege>
	//</Token>

	public class AccessTokenParser extends DefaultHandler {

		private final Deque<DefaultHandler> buildersStack = new ArrayDeque<>();

		private StringBuilder text;

		private String username;
		private String tokenId;
		private String name;
		private PasswordCrypt token;
		private ZonedDateTime validFrom;
		private ZonedDateTime validTo;
		private ZonedDateTime lastUsed;

		private Map<String, Privilege> privileges;

		public AccessTokenParser() {
			init();
		}

		private void init() {

			this.text = null;
			this.privileges = new HashMap<>();

			this.username = null;
			this.tokenId = null;
			this.name = null;
			this.token = null;
			this.validFrom = null;
			this.validTo = null;
			this.lastUsed = null;
		}

		@Override
		public void startElement(String uri, String localName, String qName, Attributes attributes)
				throws SAXException {

			this.text = new StringBuilder();

			switch (localName) {
				case TOKEN -> {
					this.username = attributes.getValue(ATTR_USERNAME).trim();
					this.tokenId = attributes.getValue(ATTR_TOKEN_ID).trim();
					this.name = attributes.getValue(ATTR_NAME).trim();
					this.token = PasswordCrypt.parse(attributes.getValue(ATTR_TOKEN).trim());
					this.validFrom = ISO8601.parseToZdt(attributes.getValue(ATTR_VALID_FROM).trim());
					this.validTo = ISO8601.parseToZdt(attributes.getValue(ATTR_VALID_TO).trim());

					String lastUsedS = attributes.getValue(ATTR_LAST_USED);
					if (StringHelper.isNotEmpty(lastUsedS))
						this.lastUsed = ISO8601.parseToZdt(lastUsedS.trim());
				}
				case PRIVILEGE -> {
					if (this.buildersStack.stream().anyMatch(e -> e.getClass().equals(PrivilegeParser.class)))
						throw new IllegalArgumentException("Previous Privilege not closed!");
					this.buildersStack.push(
							new PrivilegeParser(privilege -> this.privileges.put(privilege.name(), privilege)));
				}
			}

			if (!this.buildersStack.isEmpty())
				this.buildersStack.peek().startElement(uri, localName, qName, attributes);
		}

		@Override
		public void characters(char[] ch, int start, int length) throws SAXException {
			if (!this.buildersStack.isEmpty()) {
				this.buildersStack.peek().characters(ch, start, length);
			} else if (this.text != null) {
				this.text.append(ch, start, length);
			}
		}

		@Override
		public void endElement(String uri, String localName, String qName) throws SAXException {

			if (!this.buildersStack.isEmpty())
				this.buildersStack.peek().endElement(uri, localName, qName);

			if (localName.equals(PRIVILEGE)) {
				this.buildersStack.pop();
			}

			if (localName.equals(TOKEN)) {
				PersonalAccessToken token = new PersonalAccessToken(this.tokenId, this.username, this.name, this.token,
						this.validFrom, this.validTo, this.lastUsed, this.privileges);
				tokens.put(token.tokenId(), token);
				if (verbose)
					logger.info("New PersonalAccessToken: {}", token);
				init();
			}
		}
	}

	public static class PrivilegeParser extends DefaultHandler {

		private final Consumer<Privilege> consumer;
		private StringBuilder text;

		private String privilegeName;
		private String privilegePolicy;
		private boolean allAllowed;
		private Set<String> denyList;
		private Set<String> allowList;

		public PrivilegeParser(Consumer<Privilege> consumer) {
			this.consumer = consumer;
			init();
		}

		private void init() {

			this.text = null;

			this.privilegeName = null;
			this.privilegePolicy = null;
			this.allAllowed = false;
			this.denyList = new HashSet<>();
			this.allowList = new HashSet<>();
		}

		@Override
		public void startElement(String uri, String localName, String qName, Attributes attributes) {

			this.text = new StringBuilder();

			switch (localName) {
				case PRIVILEGE -> {
					this.privilegeName = attributes.getValue(ATTR_NAME).trim();
					this.privilegePolicy = attributes.getValue(ATTR_POLICY).trim();
				}
				case ALLOW, DENY, ALL_ALLOWED -> {
				}
				// no-op
				default -> throw new IllegalArgumentException("Unhandled tag " + localName);
			}
		}

		@Override
		public void characters(char[] ch, int start, int length) {
			if (this.text != null)
				this.text.append(ch, start, length);
		}

		@Override
		public void endElement(String uri, String localName, String qName) {
			switch (localName) {
				case ALL_ALLOWED -> this.allAllowed = StringHelper.parseBoolean(getText());
				case ALLOW -> this.allowList.add(getText());
				case DENY -> this.denyList.add(getText());
				case PRIVILEGE -> {
					Privilege privilege = new Privilege(this.privilegeName, this.privilegePolicy, this.allAllowed,
							this.denyList, this.allowList);
					this.consumer.accept(privilege);
					this.privilegeName = null;
					this.privilegePolicy = null;
					this.allAllowed = false;
					this.denyList = new HashSet<>();
					this.allowList = new HashSet<>();
				}
				default -> throw new IllegalStateException("Unexpected value: " + localName);
			}
		}

		private String getText() {
			return this.text.toString().trim();
		}
	}
}
