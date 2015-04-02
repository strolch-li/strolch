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

import java.util.Date;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import li.strolch.model.xml.Iso8601DateAdapter;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@XmlRootElement(name = "DateRange")
@XmlAccessorType(XmlAccessType.NONE)
public class DateRange {

	@XmlAttribute(name = "fromInclusive")
	private boolean fromInclusive;

	@XmlAttribute(name = "toInclusive")
	private boolean toInclusive;

	@XmlElement(name = "fromDate")
	@XmlJavaTypeAdapter(Iso8601DateAdapter.class)
	private Date fromDate;

	@XmlElement(name = "toDate")
	@XmlJavaTypeAdapter(Iso8601DateAdapter.class)
	private Date toDate;

	public boolean isFromInclusive() {
		return fromInclusive;
	}

	public void setFromInclusive(boolean fromInclusive) {
		this.fromInclusive = fromInclusive;
	}

	public boolean isToInclusive() {
		return toInclusive;
	}

	public void setToInclusive(boolean toInclusive) {
		this.toInclusive = toInclusive;
	}

	public Date getFromDate() {
		return fromDate;
	}

	public void setFromDate(Date fromDate) {
		this.fromDate = fromDate;
	}

	public Date getToDate() {
		return toDate;
	}

	public void setToDate(Date toDate) {
		this.toDate = toDate;
	}
}
