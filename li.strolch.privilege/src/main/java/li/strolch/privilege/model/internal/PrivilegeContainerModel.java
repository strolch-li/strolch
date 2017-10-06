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
package li.strolch.privilege.model.internal;

import java.lang.reflect.InvocationTargetException;
import java.text.MessageFormat;
import java.util.HashMap;
import java.util.Map;

import li.strolch.privilege.base.PrivilegeException;
import li.strolch.privilege.handler.PrivilegeHandler;
import li.strolch.privilege.policy.PrivilegePolicy;

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
	private String persistenceHandlerClassName;
	private String userChallengeHandlerClassName;
	private String ssoHandlerClassName;

	private Map<String, String> encryptionHandlerParameterMap;
	private Map<String, String> persistenceHandlerParameterMap;
	private Map<String, String> challengeHandlerParameterMap;
	private Map<String, String> ssoHandlerParameterMap;

	private Map<String, String> parameterMap;

	private Map<String, Class<PrivilegePolicy>> policies;

	public PrivilegeContainerModel() {
		this.policies = new HashMap<>();
		this.encryptionHandlerParameterMap = new HashMap<>();
		this.persistenceHandlerParameterMap = new HashMap<>();
		this.challengeHandlerParameterMap = new HashMap<>();
		this.ssoHandlerParameterMap = new HashMap<>();
	}

	public Map<String, String> getParameterMap() {
		return this.parameterMap;
	}

	public void setParameterMap(Map<String, String> parameterMap) {
		this.parameterMap = parameterMap;
	}

	public String getEncryptionHandlerClassName() {
		return this.encryptionHandlerClassName;
	}

	public void setEncryptionHandlerClassName(String encryptionHandlerClassName) {
		this.encryptionHandlerClassName = encryptionHandlerClassName;
	}

	public Map<String, String> getEncryptionHandlerParameterMap() {
		return this.encryptionHandlerParameterMap;
	}

	public void setEncryptionHandlerParameterMap(Map<String, String> encryptionHandlerParameterMap) {
		this.encryptionHandlerParameterMap = encryptionHandlerParameterMap;
	}

	public String getPersistenceHandlerClassName() {
		return this.persistenceHandlerClassName;
	}

	public void setPersistenceHandlerClassName(String persistenceHandlerClassName) {
		this.persistenceHandlerClassName = persistenceHandlerClassName;
	}

	public Map<String, String> getPersistenceHandlerParameterMap() {
		return this.persistenceHandlerParameterMap;
	}

	public void setPersistenceHandlerParameterMap(Map<String, String> persistenceHandlerParameterMap) {
		this.persistenceHandlerParameterMap = persistenceHandlerParameterMap;
	}

	public String getUserChallengeHandlerClassName() {
		return this.userChallengeHandlerClassName;
	}

	public void setUserChallengeHandlerClassName(String userChallengeHandlerClassName) {
		this.userChallengeHandlerClassName = userChallengeHandlerClassName;
	}

	public String getSsoHandlerClassName() {
		return this.ssoHandlerClassName;
	}

	public void setSsoHandlerClassName(String ssoHandlerClassName) {
		this.ssoHandlerClassName = ssoHandlerClassName;
	}

	public Map<String, String> getUserChallengeHandlerParameterMap() {
		return this.challengeHandlerParameterMap;
	}

	public void setUserChallengeHandlerParameterMap(Map<String, String> challengeHandlerParameterMap) {
		this.challengeHandlerParameterMap = challengeHandlerParameterMap;
	}

	public Map<String, String> getSsoHandlerParameterMap() {
		return this.ssoHandlerParameterMap;
	}

	public void setSsoHandlerParameterMap(Map<String, String> ssoHandlerParameterMap) {
		this.ssoHandlerParameterMap = ssoHandlerParameterMap;
	}

	public void addPolicy(String privilegeName, String policyClassName) {

		try {

			// load class and try to create a new instance
			@SuppressWarnings("unchecked")
			Class<PrivilegePolicy> clazz = (Class<PrivilegePolicy>) Class.forName(policyClassName);

			clazz.getConstructor().newInstance();

			this.policies.put(privilegeName, clazz);

		} catch (InstantiationException | InvocationTargetException e) {
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
		} catch (NoSuchMethodException e) {
			String msg = "Configured Privilege Policy {0} with class {1} has missing parameterless constructor"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, privilegeName, policyClassName);
			throw new PrivilegeException(msg, e);
		}
	}

	public Map<String, Class<PrivilegePolicy>> getPolicies() {
		return this.policies;
	}

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
		builder.append(", challengeHandlerParameterMap=");
		builder.append(this.challengeHandlerParameterMap.size());
		builder.append(", ssoHandlerParameterMap=");
		builder.append(this.ssoHandlerParameterMap.size());
		builder.append(", parameterMap=");
		builder.append(this.parameterMap.size());
		builder.append(", policies=");
		builder.append(this.policies.size());
		builder.append("]");
		return builder.toString();
	}
}
