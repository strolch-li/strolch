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

import li.strolch.privilege.base.PrivilegeException;
import li.strolch.privilege.model.internal.Role;
import li.strolch.utils.dbc.DBC;

import java.text.MessageFormat;
import java.util.List;

import static li.strolch.utils.helper.StringHelper.isEmpty;
import static li.strolch.utils.helper.StringHelper.trimOrEmpty;

/**
 * To keep certain details of the {@link Role} itself hidden from remote clients and make sure instances are only edited
 * by users with the correct privilege, this representational version is allowed to be viewed by remote clients and
 * simply wraps all public data from the {@link Role}
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class RoleRep {

	private String name;
	private List<PrivilegeRep> privileges;

	/**
	 * Default constructor
	 *
	 * @param name       the name of this role
	 * @param privileges the list of privileges granted to this role
	 */
	public RoleRep(String name, List<PrivilegeRep> privileges) {
		this.name = trimOrEmpty(name);
		this.privileges = privileges == null ? List.of() : List.copyOf(privileges);
	}

	/**
	 * validates that all required fields are set
	 */
	public void validate() {
		if (isEmpty(this.name))
			throw new PrivilegeException("name is null");

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

	/**
	 * @return the name
	 */
	public String getName() {
		return this.name;
	}

	/**
	 * @param name the name to set
	 */
	public void setName(String name) {
		this.name = trimOrEmpty(name);
	}

	/**
	 * Returns the privileges assigned to this Role as a list
	 *
	 * @return the privileges assigned to this Role as a list
	 */
	public List<PrivilegeRep> getPrivileges() {
		return this.privileges;
	}

	/**
	 * Sets the privileges on this from a list
	 *
	 * @param privileges the list of privileges to assign to this role
	 */
	public void setPrivileges(List<PrivilegeRep> privileges) {
		DBC.PRE.assertNotNull("privileges must not be null!", privileges);
		this.privileges = List.copyOf(privileges);
	}

	/**
	 * Returns a string representation of this object displaying its concrete type and its values
	 *
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return "RoleRep [name=" + this.name + ", privilegeMap=" + this.privileges + "]";
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
		if (this.name == null)
			return other.name == null;
		return this.name.equals(other.name);
	}

	public <T> T accept(PrivilegeElementVisitor<T> visitor) {
		return visitor.visitRoleRep(this);
	}
}
