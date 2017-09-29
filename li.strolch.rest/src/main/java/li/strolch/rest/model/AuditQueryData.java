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
package li.strolch.rest.model;

import javax.ws.rs.QueryParam;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class AuditQueryData {

	@QueryParam("elementType")
	private String elementType;

	@QueryParam("elementSubType")
	private String elementSubType;

	@QueryParam("elementId")
	private String elementId;

	@QueryParam("username")
	private String username;

	@QueryParam("firstname")
	private String firstname;

	@QueryParam("lastname")
	private String lastname;

	@QueryParam("action")
	private String action;

	@QueryParam("accessTypes")
	private String accessTypes;

	@QueryParam("fromDate")
	private String fromDate;

	@QueryParam("toDate")
	private String toDate;

	@QueryParam("limit")
	private long limit;

	public String getElementType() {
		return elementType;
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

	public String getElementId() {
		return elementId;
	}

	public void setElementId(String elementId) {
		this.elementId = elementId;
	}

	public String getUsername() {
		return username;
	}

	public void setUsername(String username) {
		this.username = username;
	}

	public String getFirstname() {
		return firstname;
	}

	public void setFirstname(String firstname) {
		this.firstname = firstname;
	}

	public String getLastname() {
		return lastname;
	}

	public void setLastname(String lastname) {
		this.lastname = lastname;
	}

	public String getAction() {
		return action;
	}

	public void setAction(String action) {
		this.action = action;
	}

	public String getAccessTypes() {
		return accessTypes;
	}

	public void setAccessTypes(String accessTypes) {
		this.accessTypes = accessTypes;
	}

	public String getFromDate() {
		return fromDate;
	}

	public void setFromDate(String fromDate) {
		this.fromDate = fromDate;
	}

	public String getToDate() {
		return toDate;
	}

	public void setToDate(String toDate) {
		this.toDate = toDate;
	}

	public long getLimit() {
		return limit;
	}

	public void setLimit(long limit) {
		this.limit = limit;
	}
}
