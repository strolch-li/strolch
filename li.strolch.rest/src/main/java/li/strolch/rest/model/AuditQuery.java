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

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@XmlRootElement(name = "AuditQuery")
@XmlAccessorType(XmlAccessType.NONE)
public class AuditQuery {

	@XmlAttribute(name = "elementType")
	private String elementType;

	@XmlAttribute(name = "elementSubType")
	private String elementSubType;

	@XmlAttribute(name = "elementId")
	private String elementId;

	@XmlElement(name = "identity")
	private IdentitySelection identity;

	@XmlElement(name = "action")
	private ActionSelection action;

	@XmlElement(name = "dateRange")
	private DateRange dateRange;

	@XmlElement(name = "limit")
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

	public IdentitySelection getIdentity() {
		return identity;
	}

	public void setIdentity(IdentitySelection identity) {
		this.identity = identity;
	}

	public ActionSelection getAction() {
		return action;
	}

	public void setAction(ActionSelection action) {
		this.action = action;
	}

	public DateRange getDateRange() {
		return dateRange;
	}

	public void setDateRange(DateRange dateRange) {
		this.dateRange = dateRange;
	}

	public long getLimit() {
		return limit;
	}

	public void setLimit(long limit) {
		this.limit = limit;
	}
}
