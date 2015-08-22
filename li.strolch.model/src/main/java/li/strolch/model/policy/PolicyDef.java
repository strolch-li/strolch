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
package li.strolch.model.policy;

import li.strolch.exception.StrolchPolicyException;

/**
 * <p>
 * A Policy definition defines a reference to delegation
 * </p>
 * 
 * <p>
 * The {@link #getType()} defines the policy interface which the value references a concrete implementation
 * </p>
 * 
 * <p>
 * To actually resolve a policy, use a {@link PolicyDefVisitor} which handles difference resolving strategies
 * </p>
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public abstract class PolicyDef {
	protected String type;
	protected String value;

	/**
	 * Create a new policy definition
	 * 
	 * @param type
	 * @param value
	 */
	public PolicyDef(String type, String value) {
		super();
		this.type = type;
		this.value = value;
	}

	/**
	 * Returns the type of policy being referenced, i.e. the interface
	 * 
	 * @return the type of policy being referenced, i.e. the interface
	 */
	public String getType() {
		return this.type;
	}

	/**
	 * Returns which policy instance is being referenced
	 * 
	 * @return which policy instance is being referenced
	 */
	public String getValue() {
		return this.value;
	}

	/**
	 * Resolves an instance to the policy. The {@link PolicyDefVisitor} handles the resolving of an actual policy
	 * implementation instantiating the found class and returning a new instance
	 * 
	 * @param visitor
	 * @return
	 */
	public abstract <T> Class<T> accept(PolicyDefVisitor visitor) throws ClassNotFoundException;

	/**
	 * Returns the value formatted for XML marshalling, so that the {@link #valueOf(String, String)} can then again
	 * parse the value and instantiate a concrete {@link PolicyDef} instance
	 * 
	 * @return the value formatted for XML marshalling
	 */
	public abstract String getValueForXml();

	/**
	 * Return a clone of this {@link PolicyDef} instance
	 * 
	 * @return a clone of this {@link PolicyDef} instance
	 */
	public abstract PolicyDef getClone();

	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append("PolicyDef [type=");
		sb.append(this.type);
		sb.append(", value=");
		sb.append(this.value);
		sb.append("]");
		return sb.toString();
	}

	/**
	 * Returns a {@link PolicyDef} instance which handles the given type of XML Value
	 * 
	 * @param type
	 *            the type
	 * @param xmlValue
	 *            the XML formatted value with the prefix denoting the {@link PolicyDef} type
	 * 
	 * @return a {@link PolicyDef} instance which handles the given type of XML Value
	 */
	public static PolicyDef valueOf(String type, String xmlValue) {

		if (xmlValue.startsWith(JavaPolicyDef.XML_PREFIX)) {

			String value = xmlValue.substring(JavaPolicyDef.XML_PREFIX.length());

			try {
				Class.forName(value);

			} catch (ClassNotFoundException e) {
				throw new StrolchPolicyException("Invalid policy configuration. Policy does not exist: " + value);
			}

			return new JavaPolicyDef(type, value);

		} else if (xmlValue.startsWith(KeyPolicyDef.XML_PREFIX)) {

			String value = xmlValue.substring(KeyPolicyDef.XML_PREFIX.length());
			return new KeyPolicyDef(type, value);

		} else {
			throw new StrolchPolicyException("Unhandled PolicyDef from xml value " + xmlValue);
		}
	}
}