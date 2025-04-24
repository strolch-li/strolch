/*
 * Copyright (c) 2013-2024 Robert von Burg <eitch@eitchnet.ch>
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

import com.google.gson.JsonElement;

import java.util.Date;

import static li.strolch.utils.helper.StringHelper.hashSha256AsHex;

/**
 * Used to log/audit access to the agent
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class Audit implements Comparable<Audit> {

	private Long id;
	private String username;
	private Date date;
	private String elementType;
	private String elementSubType;
	private String elementAccessed;
	private Date newVersion;
	private String action;
	private AccessType accessType;
	private JsonElement additionalData;

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

	public Date getNewVersion() {
		return this.newVersion;
	}

	public void setElementAccessed(String elementAccessed) {
		this.elementAccessed = elementAccessed;
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

	public JsonElement getAdditionalData() {
		return this.additionalData;
	}

	public void setAdditionalData(JsonElement additionalData) {
		this.additionalData = additionalData;
	}

	public void setAccessType(AccessType accessType) {
		this.accessType = accessType;
	}

	public <U> U accept(AuditVisitor<U> visitor) {
		return visitor.visitAudit(this);
	}

	public String buildRelevantHash() {
		String builder = this.username
				+ this.elementType
				+ this.elementSubType
				+ this.elementAccessed
				+ this.action
				+ this.accessType
				+ this.additionalData;
		return hashSha256AsHex(builder);
	}

	@Override
	public String toString() {
		return "Audit{"
				+ "id="
				+ id
				+ ", username='"
				+ username
				+ '\''
				+ ", date="
				+ date
				+ ", elementType='"
				+ elementType
				+ '\''
				+ ", elementSubType='"
				+ elementSubType
				+ '\''
				+ ", elementAccessed='"
				+ elementAccessed
				+ '\''
				+ ", action='"
				+ action
				+ '\''
				+ ", accessType="
				+ accessType
				+ '}';
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
