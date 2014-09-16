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
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import li.strolch.model.Order;
import li.strolch.model.State;
import ch.eitchnet.utils.iso8601.ISO8601FormatFactory;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement(name = "Order")
@XmlType(name = "")
public class OrderDetail extends GroupedParameterizedElementDetail {

	@XmlAttribute(name = "date")
	private String date;
	@XmlAttribute(name = "state")
	private State state;

	public OrderDetail() {
		// no-arg constructor for JAXB
	}

	/**
	 * @param id
	 * @param name
	 * @param type
	 * @param date
	 * @param state
	 * @param parameterizedElementDetails
	 */
	public OrderDetail(String id, String name, String type, Date date, State state,
			List<ParameterizedElementDetail> parameterizedElementDetails) {
		super(id, name, type, parameterizedElementDetails);
		this.state = state;
		this.date = ISO8601FormatFactory.getInstance().formatDate(date);
	}

	/**
	 * @param order
	 */
	public OrderDetail(Order order) {
		super(order);
		this.state = order.getState();
		this.date = ISO8601FormatFactory.getInstance().formatDate(order.getDate());
	}

	/**
	 * @return the date
	 */
	public String getDate() {
		return this.date;
	}

	/**
	 * @param date
	 *            the date to set
	 */
	public void setDate(String date) {
		this.date = date;
	}

	/**
	 * @return the state
	 */
	public State getState() {
		return this.state;
	}

	/**
	 * @param state
	 *            the state to set
	 */
	public void setState(State state) {
		this.state = state;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + ((this.date == null) ? 0 : this.date.hashCode());
		result = prime * result + ((this.state == null) ? 0 : this.state.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (!super.equals(obj))
			return false;
		if (getClass() != obj.getClass())
			return false;
		OrderDetail other = (OrderDetail) obj;
		if (this.date == null) {
			if (other.date != null)
				return false;
		} else if (!this.date.equals(other.date))
			return false;
		if (this.state != other.state)
			return false;
		return true;
	}
}
