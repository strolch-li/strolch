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

import java.io.Serializable;
import java.util.Date;

import li.strolch.model.StrolchRootElement;

/**
 * Used to log/audit access to {@link StrolchRootElement}
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class Audit implements Comparable<Audit>, Serializable {

	private static final long serialVersionUID = 1L;

	private Long id;
	private String username;
	private String firstname;
	private String lastname;
	private Date date;

	private String elementType;
	private String elementAccessed;
	private Date newVersion;

	private String action;
	private AccessType accessType;

	/**
	 * @return the id
	 */
	public Long getId() {
		return this.id;
	}

	/**
	 * @param id
	 *            the id to set
	 */
	public void setId(long id) {
		this.id = id;
	}

	/**
	 * @return the username
	 */
	public String getUsername() {
		return this.username;
	}

	/**
	 * @param username
	 *            the username to set
	 */
	public void setUsername(String username) {
		this.username = username;
	}

	/**
	 * @return the firstname
	 */
	public String getFirstname() {
		return this.firstname;
	}

	/**
	 * @param firstname
	 *            the firstname to set
	 */
	public void setFirstname(String firstname) {
		this.firstname = firstname;
	}

	/**
	 * @return the lastname
	 */
	public String getLastname() {
		return this.lastname;
	}

	/**
	 * @param lastname
	 *            the lastname to set
	 */
	public void setLastname(String lastname) {
		this.lastname = lastname;
	}

	/**
	 * @return the date
	 */
	public Date getDate() {
		return this.date;
	}

	/**
	 * @param date
	 *            the date to set
	 */
	public void setDate(Date date) {
		this.date = date;
	}

	/**
	 * @return the elementType
	 */
	public String getElementType() {
		return this.elementType;
	}

	/**
	 * @param elementType
	 *            the elementType to set
	 */
	public void setElementType(String elementType) {
		this.elementType = elementType;
	}

	/**
	 * @return the elementAccessed
	 */
	public String getElementAccessed() {
		return this.elementAccessed;
	}

	/**
	 * @param elementAccessed
	 *            the elementAccessed to set
	 */
	public void setElementAccessed(String elementAccessed) {
		this.elementAccessed = elementAccessed;
	}

	/**
	 * @return the newVersion
	 */
	public Date getNewVersion() {
		return this.newVersion;
	}

	/**
	 * @param newVersion
	 *            the newVersion to set
	 */
	public void setNewVersion(Date newVersion) {
		this.newVersion = newVersion;
	}

	/**
	 * @return the action
	 */
	public String getAction() {
		return this.action;
	}

	/**
	 * @param action
	 *            the action to set
	 */
	public void setAction(String action) {
		this.action = action;
	}

	/**
	 * @return the accessType
	 */
	public AccessType getAccessType() {
		return this.accessType;
	}

	/**
	 * @param accessType
	 *            the accessType to set
	 */
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
			if (other.id != null) {
				return false;
			}
		} else if (!this.id.equals(other.id)) {
			return false;
		}
		return true;
	}

	@Override
	public int compareTo(Audit o) {
		return getId().compareTo(o.getId());
	}
}
