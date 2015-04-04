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

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import li.strolch.model.StrolchRootElement;
import li.strolch.model.xml.Iso8601DateAdapter;

/**
 * Used to log/audit access to {@link StrolchRootElement}
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@XmlRootElement(name = "Audit")
@XmlAccessorType(XmlAccessType.NONE)
public class Audit implements Comparable<Audit>, Serializable {

	private static final long serialVersionUID = 1L;

	@XmlAttribute(name = "id")
	private Long id;

	@XmlAttribute(name = "username")
	private String username;

	@XmlAttribute(name = "firstname")
	private String firstname;

	@XmlAttribute(name = "lastname")
	private String lastname;

	@XmlElement(name = "date")
	@XmlJavaTypeAdapter(Iso8601DateAdapter.class)
	private Date date;

	@XmlAttribute(name = "elementType")
	private String elementType;

	@XmlAttribute(name = "elementSubType")
	private String elementSubType;

	@XmlAttribute(name = "elementAccessed")
	private String elementAccessed;

	@XmlElement(name = "newVersion")
	@XmlJavaTypeAdapter(Iso8601DateAdapter.class)
	private Date newVersion;

	@XmlAttribute(name = "action")
	private String action;

	@XmlAttribute(name = "accessType")
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
