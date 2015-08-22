/*
 * Copyright 2015 Robert von Burg <eitch@eitchnet.ch>
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
package li.strolch.policy;

import java.util.HashMap;
import java.util.Map;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

import ch.eitchnet.utils.dbc.DBC;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class StrolchPolicyFileParser extends DefaultHandler {

	public static final String POLICY = "Policy";
	public static final String POLICY_TYPE = "PolicyType";

	public static final String TYPE = "Type";
	public static final String API = "Api";

	public static final String KEY = "Key";
	public static final String CLASS = "Class";

	private PolicyModel policyModel;
	private PolicyType policyType;

	public StrolchPolicyFileParser() {
		this.policyModel = new PolicyModel();
	}

	@Override
	public void startElement(String uri, String localName, String qName, Attributes attributes) throws SAXException {
		switch (qName) {
		case POLICY_TYPE:

			String type = attributes.getValue(TYPE);
			String api = attributes.getValue(API);

			DBC.PRE.assertNotEmpty(TYPE + " not defined on a " + POLICY_TYPE, type);
			DBC.PRE.assertNotEmpty(API + " not defined on a " + POLICY_TYPE, api);

			this.policyType = new PolicyType(type, api);
			this.policyModel.getPolicyTypes().put(type, policyType);

			break;

		case POLICY:

			String key = attributes.getValue(KEY);
			String clazz = attributes.getValue(CLASS);

			DBC.PRE.assertNotEmpty(KEY + " not defined on a " + POLICY_TYPE, key);
			DBC.PRE.assertNotEmpty(CLASS + " not defined on a " + POLICY_TYPE, clazz);

			this.policyType.getPolicyByKeyMap().put(key, clazz);

			break;
		default:
			break;
		}
	}

	@Override
	public void endElement(String uri, String localName, String qName) throws SAXException {
		switch (qName) {
		case POLICY_TYPE:
			this.policyType = null;
			break;
		default:
			break;
		}
	}

	public PolicyModel getPolicyModel() {
		return this.policyModel;
	}

	public class PolicyModel {
		private Map<String, PolicyType> policyTypes;

		public PolicyModel() {
			this.policyTypes = new HashMap<>();
		}

		public Map<String, PolicyType> getPolicyTypes() {
			return this.policyTypes;
		}
	}

	public class PolicyType {
		private String type;
		private String api;
		private Map<String, String> policyByKeyMap;

		public PolicyType(String type, String api) {
			super();
			this.type = type;
			this.api = api;
			this.policyByKeyMap = new HashMap<>();
		}

		public String getApi() {
			return this.api;
		}

		public String getType() {
			return this.type;
		}

		public Map<String, String> getPolicyByKeyMap() {
			return this.policyByKeyMap;
		}
	}
}
