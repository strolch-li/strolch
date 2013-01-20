/*
 * Copyright (c) 2012, Robert von Burg
 *
 * All rights reserved.
 *
 * This file is part of the Privilege.
 *
 *  Privilege is free software: you can redistribute 
 *  it and/or modify it under the terms of the GNU General Public License as 
 *  published by the Free Software Foundation, either version 3 of the License, 
 *  or (at your option) any later version.
 *
 *  Privilege is distributed in the hope that it will 
 *  be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with Privilege.  If not, see 
 *  <http://www.gnu.org/licenses/>.
 */
package ch.eitchnet.privilege.model.internal;

import java.util.HashMap;
import java.util.Map;

import ch.eitchnet.privilege.base.PrivilegeException;
import ch.eitchnet.privilege.policy.PrivilegePolicy;

public class PrivilegeContainerModel {

	String encryptionHandlerClassName;
	Map<String, String> encryptionHandlerParameterMap;
	String persistenceHandlerClassName;
	Map<String, String> persistenceHandlerParameterMap;
	Map<String, String> parameterMap;

	private Map<String, Class<PrivilegePolicy>> policies = new HashMap<String, Class<PrivilegePolicy>>();

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
	 * @param name
	 * @param policyClass
	 */
	public void addPolicy(String privilegeName, String policyClassName) {

		try {

			// load class and try to create a new instance
			@SuppressWarnings("unchecked")
			Class<PrivilegePolicy> clazz = (Class<PrivilegePolicy>) Class.forName(policyClassName);
			clazz.newInstance();

			this.policies.put(privilegeName, clazz);

		} catch (InstantiationException e) {
			throw new PrivilegeException("Configured Privilege Policy " + privilegeName + " with class "
					+ policyClassName + " could not be instantiated.", e);
		} catch (IllegalAccessException e) {
			throw new PrivilegeException("Configured Privilege Policy " + privilegeName + " with class "
					+ policyClassName + " can not be accessed.", e);
		} catch (ClassNotFoundException e) {
			throw new PrivilegeException("Configured Privilege Policy " + privilegeName + " with class "
					+ policyClassName + " does not exist.", e);
		}
	}

	/**
	 * @return the policies
	 */
	public Map<String, Class<PrivilegePolicy>> getPolicies() {
		return this.policies;
	}

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