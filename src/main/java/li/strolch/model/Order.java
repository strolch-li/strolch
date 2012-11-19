/*
 * Copyright (c) 2012, Robert von Burg
 *
 * All rights reserved.
 *
 * This file is part of li.strolch.model.
 *
 *  li.strolch.model is free software: you can redistribute 
 *  it and/or modify it under the terms of the GNU General Public License as 
 *  published by the Free Software Foundation, either version 3 of the License, 
 *  or (at your option) any later version.
 *
 *  li.strolch.model is distributed in the hope that it will 
 *  be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with li.strolch.model.  If not, see 
 *  <http://www.gnu.org/licenses/>.
 */
package li.strolch.model;

import li.strolch.model.Locator.LocatorBuilder;

import org.dom4j.Element;
import org.dom4j.tree.DefaultElement;

/**
 * The Order is an object used in the EDF to transfer data from one range to another. Orders are not to be thought of as
 * Resources. Resources are supposed to be thought of as things i.e. a table, a machine and so forth, where a order is
 * to be thought of as an object for doing something.
 * 
 * In this sense, orders do not need to be verified, so all verifier chracteristics are disabled and the
 * getVerifier()-method will return the null reference
 * 
 * @author eitch
 * 
 */
public class Order extends ParameterizedElement {

	private static final long serialVersionUID = 0L;

	public static final String PREFIX_ORDER = "OrderPrefix";

	private long date;
	private State state;

	/**
	 * Empty constructor
	 */
	public Order() {
		super();
	}

	/**
	 * Build an Order
	 * 
	 * @param id
	 * @param name
	 * @param type
	 */
	public Order(String id, String name, String type) {
		super(id, name, type);

		setState(State.CREATED);
		setDate(System.currentTimeMillis());
	}

	/**
	 * Build an Order
	 * 
	 * @param id
	 * @param name
	 * @param type
	 * @param date
	 * @param state
	 */
	public Order(String id, String name, String type, long date, State state) {
		super(id, name, type);

		setState(state);
		setDate(date);
	}

	/**
	 * Builds this Order from a Dom4j element
	 * 
	 * @param element
	 */
	public Order(Element element) {
		super.fromDom(element);

		String date = element.attributeValue("Date");
		String state = element.attributeValue("State");

		// TODO the format should be globally configured
		if (date == null || date.isEmpty()) {
			setDate(0);
		} else {
			setDate(Long.parseLong(date));
		}

		if (state == null || state.isEmpty()) {
			setState(State.CREATED);
		} else {
			setState(State.valueOf(state));
		}
	}

	/**
	 * @return the date
	 */
	public long getDate() {
		return this.date;
	}

	/**
	 * @param date
	 *            the date to set
	 */
	public void setDate(long date) {
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

	/**
	 * @see li.strolch.StrolchElement.datalandscape.element.IEdpElement#toDom()
	 */
	@Override
	public Element toDom() {

		Element orderElement = new DefaultElement("Order");
		fillElement(orderElement);

		// TODO the format should be globally configured
		orderElement.addAttribute("Date", Long.toString(this.date));
		orderElement.addAttribute("State", this.state.toString());

		return orderElement;
	}

	@Override
	public Order getClone() {
		Order clone = new Order();

		super.fillClone(clone);

		clone.setDate(this.date);
		clone.setState(this.state);

		return clone;
	}

	@Override
	protected void fillLocator(LocatorBuilder lb) {
		lb.append("Order").append(getId());
	}

	@Override
	public Locator getLocator() {
		LocatorBuilder lb = new LocatorBuilder();
		fillLocator(lb);
		return lb.build();
	}

	@Override
	public String toString() {

		StringBuilder builder = new StringBuilder();

		builder.append("Order [id=");
		builder.append(this.id);
		builder.append(", name=");
		builder.append(this.name);
		builder.append(", type=");
		builder.append(this.type);
		builder.append(", state=");
		builder.append(this.state);
		// TODO the format should be globally configured
		builder.append(", date=");
		builder.append(this.date);
		builder.append("]");

		return builder.toString();
	}
}
