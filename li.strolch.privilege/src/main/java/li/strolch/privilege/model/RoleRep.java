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
package li.strolch.privilege.model;

import java.io.Serializable;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import li.strolch.privilege.base.PrivilegeException;
import li.strolch.privilege.model.internal.Role;
import li.strolch.utils.helper.StringHelper;

/**
 * To keep certain details of the {@link Role} itself hidden from remote clients and make sure instances are only edited
 * by users with the correct privilege, this representational version is allowed to be viewed by remote clients and
 * simply wraps all public data from the {@link Role}
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@XmlRootElement(name = "Role")
@XmlAccessorType(XmlAccessType.NONE)
public class RoleRep implements Serializable {

	private static final long serialVersionUID = 1L;

	@XmlAttribute(name = "name")
	private String name;

	@XmlElement(name = "privileges")
	private List<PrivilegeRep> privileges;

	/**
	 * Default constructor
	 * 
	 * @param name
	 *            the name of this role
	 * @param privileges
	 *            the list of privileges granted to this role
	 */
	public RoleRep(String name, List<PrivilegeRep> privileges) {
		this.name = name;
		this.privileges = privileges;
	}

	/**
	 * 
	 */
	@SuppressWarnings("unused")
	private RoleRep() {
		// no-arg constructor for JAXB
	}

	/**
	 * validates that all required fields are set
	 */
	public void validate() {
		if (StringHelper.isEmpty(this.name))
			throw new PrivilegeException("name is null"); //$NON-NLS-1$

		if (this.privileges != null && !this.privileges.isEmpty()) {
			for (PrivilegeRep privilege : this.privileges) {
				try {
					privilege.validate();
				} catch (Exception e) {
					String msg = "Privilege {0} is invalid on role {1}";
					msg = MessageFormat.format(msg, privilege.getName(), this.name);
					throw new PrivilegeException(msg, e);
				}
			}
		}
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
	 * Returns the privileges assigned to this Role as a list
	 * 
	 * @return the privileges assigned to this Role as a list
	 */
	public List<PrivilegeRep> getPrivileges() {
		return this.privileges == null ? new ArrayList<>() : this.privileges;
	}

	/**
	 * Sets the privileges on this from a list
	 * 
	 * @param privileges
	 *            the list of privileges to assign to this role
	 */
	public void setPrivileges(List<PrivilegeRep> privileges) {
		this.privileges = privileges;
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
		builder.append((this.privileges == null ? "null" : this.privileges));
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
