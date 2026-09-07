/*
 * Copyright (c) 2015-2025 Robert von Burg <eitch@eitchnet.ch>
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

import li.strolch.utils.dbc.DBC;
import org.xml.sax.Attributes;
import org.xml.sax.helpers.DefaultHandler;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static li.strolch.policy.PolicySerializationConstants.*;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class StrolchPolicyFileParser extends DefaultHandler {

	private final PolicyModel policyModel;
	private PolicyType policyType;

	public StrolchPolicyFileParser() {
		this.policyModel = new PolicyModel();
	}

	@Override
	public void startElement(String uri, String localName, String qName, Attributes attributes) {
		switch (localName) {
			case POLICY_TYPE -> {
				String type = attributes.getValue(TYPE);
				String api = attributes.getValue(API);
				DBC.PRE.assertNotEmpty(TYPE + " not defined on a " + POLICY_TYPE, type);
				DBC.PRE.assertNotEmpty(API + " not defined on a " + POLICY_TYPE, api);
				this.policyType = new PolicyType(type, api);
				this.policyModel.getPolicyTypes().put(type, policyType);
			}
			case POLICY -> {
				String key = attributes.getValue(KEY);
				String clazz = attributes.getValue(CLASS);
				DBC.PRE.assertNotEmpty(KEY + " not defined on a " + POLICY_TYPE, key);
				DBC.PRE.assertNotEmpty(CLASS + " not defined on a " + POLICY_TYPE, clazz);
				this.policyType.getPolicyByKeyMap().put(key, clazz);
			}
			default -> {
			}
		}
	}

	@Override
	public void endElement(String uri, String localName, String qName) {
		if (localName.equals(POLICY_TYPE)) {
			this.policyType = null;
		}
	}

	public PolicyModel getPolicyModel() {
		return this.policyModel;
	}

	public static class PolicyModel {
		private final Map<String, PolicyType> policyTypes;

		public PolicyModel() {
			this.policyTypes = new HashMap<>();
		}

		public Map<String, PolicyType> getPolicyTypes() {
			return this.policyTypes;
		}
	}

	public static class PolicyType {
		private final String type;
		private final String api;
		private final Map<String, String> policyByKeyMap;
		private final List<String> possibleImplementations;

		public PolicyType(String type, String api) {
			super();
			this.type = type;
			this.api = api;
			this.policyByKeyMap = new HashMap<>();
			this.possibleImplementations = new ArrayList<>();
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

		public List<String> getPossibleImplementations() {
			return this.possibleImplementations;
		}

		public void addPossibleImplementation(String implementation) {
			if (!this.possibleImplementations.contains(implementation))
				this.possibleImplementations.add(implementation);
		}
	}
}
