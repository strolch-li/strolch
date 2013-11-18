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

import java.util.Date;

import li.strolch.model.Locator.LocatorBuilder;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

import ch.eitchnet.utils.helper.StringHelper;
import ch.eitchnet.utils.iso8601.ISO8601FormatFactory;

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
public class Order extends GroupedParameterizedElement {

	private static final long serialVersionUID = 0L;

	private Date date;
	private State state;

	/**
	 * Empty constructor
	 */
	public Order() {
		//
	}

	/**
	 * Default Constructor
	 * 
	 * @param id
	 * @param name
	 * @param type
	 */
	public Order(String id, String name, String type) {
		super(id, name, type);

		setState(State.CREATED);
		setDate(new Date());
	}

	/**
	 * Extended Constructor for date and {@link State}
	 * 
	 * @param id
	 * @param name
	 * @param type
	 * @param date
	 * @param state
	 */
	public Order(String id, String name, String type, Date date, State state) {
		super(id, name, type);

		setState(state);
		setDate(date);
	}

	/**
	 * DOM Constructor
	 * 
	 * @param element
	 */
	public Order(Element element) {
		super.fromDom(element);

		String date = element.getAttribute(Tags.DATE);
		String state = element.getAttribute(Tags.STATE);

		if (StringHelper.isEmpty(date)) {
			setDate(ISO8601FormatFactory.getInstance().getDateFormat().parse("-")); //$NON-NLS-1$
		} else {
			setDate(ISO8601FormatFactory.getInstance().getDateFormat().parse(date));
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
	public Element toDom(Document doc) {

		Element orderElement = doc.createElement(Tags.ORDER);
		fillElement(orderElement);

		orderElement.setAttribute(Tags.DATE, ISO8601FormatFactory.getInstance().formatDate(this.date));
		orderElement.setAttribute(Tags.STATE, this.state.toString());

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
		lb.append(Tags.ORDER).append(getId());
	}

	@Override
	public Locator getLocator() {
		LocatorBuilder lb = new LocatorBuilder();
		fillLocator(lb);
		return lb.build();
	}

	@SuppressWarnings("nls")
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
		builder.append(", date=");
		builder.append(ISO8601FormatFactory.getInstance().formatDate(this.date));
		builder.append("]");

		return builder.toString();
	}
}
