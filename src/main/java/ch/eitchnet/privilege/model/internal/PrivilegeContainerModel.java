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
package ch.eitchnet.privilege.model.internal;

import java.text.MessageFormat;
import java.util.HashMap;
import java.util.Map;

import ch.eitchnet.privilege.base.PrivilegeException;
import ch.eitchnet.privilege.handler.PrivilegeHandler;
import ch.eitchnet.privilege.policy.PrivilegePolicy;

/**
 * This class is used during XML parsing to hold the model before it is properly validated and made accessible through
 * the {@link PrivilegeHandler}
 * 
 * <p>
 * Note: This is an internal object which is not to be serialized or passed to clients
 * </p>
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class PrivilegeContainerModel {

	private String encryptionHandlerClassName;
	private Map<String, String> encryptionHandlerParameterMap;
	private String persistenceHandlerClassName;
	private Map<String, String> persistenceHandlerParameterMap;
	private Map<String, String> parameterMap;
	private Map<String, Class<PrivilegePolicy>> policies;

	/**
	 * Default constructor
	 */
	public PrivilegeContainerModel() {
		this.policies = new HashMap<String, Class<PrivilegePolicy>>();
	}

	/**
	 * @return the parameterMap
	 */
	public Map<String, String> getParameterMap() {
		return this.parameterMap;
	}

	/**
	 * @param parameterMap
	 *            the parameterMap to set
	 */
	public void setParameterMap(Map<String, String> parameterMap) {
		this.parameterMap = parameterMap;
	}

	/**
	 * @return the encryptionHandlerClassName
	 */
	public String getEncryptionHandlerClassName() {
		return this.encryptionHandlerClassName;
	}

	/**
	 * @param encryptionHandlerClassName
	 *            the encryptionHandlerClassName to set
	 */
	public void setEncryptionHandlerClassName(String encryptionHandlerClassName) {
		this.encryptionHandlerClassName = encryptionHandlerClassName;
	}

	/**
	 * @return the encryptionHandlerParameterMap
	 */
	public Map<String, String> getEncryptionHandlerParameterMap() {
		return this.encryptionHandlerParameterMap;
	}

	/**
	 * @param encryptionHandlerParameterMap
	 *            the encryptionHandlerParameterMap to set
	 */
	public void setEncryptionHandlerParameterMap(Map<String, String> encryptionHandlerParameterMap) {
		this.encryptionHandlerParameterMap = encryptionHandlerParameterMap;
	}

	/**
	 * @return the persistenceHandlerClassName
	 */
	public String getPersistenceHandlerClassName() {
		return this.persistenceHandlerClassName;
	}

	/**
	 * @param persistenceHandlerClassName
	 *            the persistenceHandlerClassName to set
	 */
	public void setPersistenceHandlerClassName(String persistenceHandlerClassName) {
		this.persistenceHandlerClassName = persistenceHandlerClassName;
	}

	/**
	 * @return the persistenceHandlerParameterMap
	 */
	public Map<String, String> getPersistenceHandlerParameterMap() {
		return this.persistenceHandlerParameterMap;
	}

	/**
	 * @param persistenceHandlerParameterMap
	 *            the persistenceHandlerParameterMap to set
	 */
	public void setPersistenceHandlerParameterMap(Map<String, String> persistenceHandlerParameterMap) {
		this.persistenceHandlerParameterMap = persistenceHandlerParameterMap;
	}

	/**
	 * @param privilegeName
	 * @param policyClassName
	 */
	public void addPolicy(String privilegeName, String policyClassName) {

		try {

			// load class and try to create a new instance
			@SuppressWarnings("unchecked")
			Class<PrivilegePolicy> clazz = (Class<PrivilegePolicy>) Class.forName(policyClassName);
			clazz.newInstance();

			this.policies.put(privilegeName, clazz);

		} catch (InstantiationException e) {
			String msg = "Configured Privilege Policy {0} with class {1} could not be instantiated."; //$NON-NLS-1$
			msg = MessageFormat.format(msg, privilegeName, policyClassName);
			throw new PrivilegeException(msg, e);
		} catch (IllegalAccessException e) {
			String msg = "Configured Privilege Policy {0} with class {1} can not be accessed."; //$NON-NLS-1$
			msg = MessageFormat.format(msg, privilegeName, policyClassName);
			throw new PrivilegeException(msg, e);
		} catch (ClassNotFoundException e) {
			String msg = "Configured Privilege Policy {0} with class {1} does not exist."; //$NON-NLS-1$
			msg = MessageFormat.format(msg, privilegeName, policyClassName);
			throw new PrivilegeException(msg, e);
		}
	}

	/**
	 * @return the policies
	 */
	public Map<String, Class<PrivilegePolicy>> getPolicies() {
		return this.policies;
	}

	/**
	 * Returns a string representation of this object displaying its concrete type and its values
	 * 
	 * @see java.lang.Object#toString()
	 */
	@SuppressWarnings("nls")
	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("PrivilegeContainerModel [encryptionHandlerClassName=");
		builder.append(this.encryptionHandlerClassName);
		builder.append(", encryptionHandlerParameterMap=");
		builder.append(this.encryptionHandlerParameterMap.size());
		builder.append(", persistenceHandlerClassName=");
		builder.append(this.persistenceHandlerClassName);
		builder.append(", persistenceHandlerParameterMap=");
		builder.append(this.persistenceHandlerParameterMap.size());
		builder.append(", parameterMap=");
		builder.append(this.parameterMap.size());
		builder.append(", policies=");
		builder.append(this.policies.size());
		builder.append("]");
		return builder.toString();
	}
}
