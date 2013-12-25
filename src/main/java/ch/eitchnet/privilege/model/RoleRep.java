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
package ch.eitchnet.privilege.model;

import java.io.Serializable;
import java.util.Map;

import ch.eitchnet.privilege.base.PrivilegeException;
import ch.eitchnet.privilege.model.internal.Role;
import ch.eitchnet.utils.helper.StringHelper;

/**
 * To keep certain details of the {@link Role} itself hidden from remote clients and make sure instances are only edited
 * by users with the correct privilege, this representational version is allowed to be viewed by remote clients and
 * simply wraps all public data from the {@link Role}
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class RoleRep implements Serializable {

	private static final long serialVersionUID = 1L;

	private String name;
	private Map<String, PrivilegeRep> privilegeMap;

	/**
	 * Default constructor
	 * 
	 * @param name
	 *            the name of this role
	 * @param privilegeMap
	 *            the map of privileges granted to this role
	 */
	public RoleRep(String name, Map<String, PrivilegeRep> privilegeMap) {

		this.name = name;
		this.privilegeMap = privilegeMap;

		validate();
	}

	/**
	 * validates that all required fields are set
	 */
	public void validate() {
		if (StringHelper.isEmpty(this.name))
			throw new PrivilegeException("name is null"); //$NON-NLS-1$
	}

	/**
	 * @return the name
	 */
	public String getName() {
		return this.name;
	}

	/**
	 * @param name
	 *            the name to set
	 */
	public void setName(String name) {
		this.name = name;
	}

	/**
	 * @return the privilegeMap
	 */
	public Map<String, PrivilegeRep> getPrivilegeMap() {
		return this.privilegeMap;
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
		builder.append("RoleRep [name=");
		builder.append(this.name);
		builder.append(", privilegeMap=");
		builder.append((this.privilegeMap == null ? "null" : this.privilegeMap));
		builder.append("]");
		return builder.toString();
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((this.name == null) ? 0 : this.name.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		RoleRep other = (RoleRep) obj;
		if (this.name == null) {
			if (other.name != null)
				return false;
		} else if (!this.name.equals(other.name))
			return false;
		return true;
	}
}
