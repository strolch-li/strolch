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
package li.strolch.model.audit;

import java.util.Date;

import li.strolch.model.StrolchRootElement;

/**
 * Used to log/audit access to {@link StrolchRootElement}
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class Audit implements Comparable<Audit> {

	private Long id;
	private String username;
	private String firstname;
	private String lastname;
	private Date date;
	private String elementType;
	private String elementSubType;
	private String elementAccessed;
	private Date newVersion;
	private String action;
	private AccessType accessType;

	public Long getId() {
		return this.id;
	}

	public void setId(long id) {
		this.id = id;
	}

	public String getUsername() {
		return this.username;
	}

	public void setUsername(String username) {
		this.username = username;
	}

	public String getFirstname() {
		return this.firstname;
	}

	public void setFirstname(String firstname) {
		this.firstname = firstname;
	}

	public String getLastname() {
		return this.lastname;
	}

	public void setLastname(String lastname) {
		this.lastname = lastname;
	}

	public Date getDate() {
		return this.date;
	}

	public void setDate(Date date) {
		this.date = date;
	}

	public String getElementType() {
		return this.elementType;
	}

	public void setElementType(String elementType) {
		this.elementType = elementType;
	}

	public String getElementSubType() {
		return elementSubType;
	}

	public void setElementSubType(String elementSubType) {
		this.elementSubType = elementSubType;
	}

	public String getElementAccessed() {
		return this.elementAccessed;
	}

	public void setElementAccessed(String elementAccessed) {
		this.elementAccessed = elementAccessed;
	}

	public Date getNewVersion() {
		return this.newVersion;
	}

	public void setNewVersion(Date newVersion) {
		this.newVersion = newVersion;
	}

	public String getAction() {
		return this.action;
	}

	public void setAction(String action) {
		this.action = action;
	}

	public AccessType getAccessType() {
		return this.accessType;
	}

	public void setAccessType(AccessType accessType) {
		this.accessType = accessType;
	}

	public <U> U accept(AuditVisitor<U> visitor) {
		return visitor.visitAudit(this);
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((this.id == null) ? 0 : this.id.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (obj == null) {
			return false;
		}
		if (getClass() != obj.getClass()) {
			return false;
		}
		Audit other = (Audit) obj;
		if (this.id == null) {
			return other.id == null;
		} else
			return this.id.equals(other.id);
	}

	@Override
	public int compareTo(Audit o) {
		return getId().compareTo(o.getId());
	}
}
