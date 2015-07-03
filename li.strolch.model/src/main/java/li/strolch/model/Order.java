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
package li.strolch.model;

import java.util.Date;

import li.strolch.model.Locator.LocatorBuilder;
import li.strolch.model.visitor.StrolchRootElementVisitor;
import ch.eitchnet.utils.iso8601.ISO8601FormatFactory;

/**
 * The Order is an object used in the EDF to transfer data from one range to another. Orders are not to be thought of as
 * Resources. Resources are supposed to be thought of as things i.e. a table, a machine and so forth, where a order is
 * to be thought of as an object for doing something.
 *
 * In this sense, orders do not need to be verified, so all verifier chracteristics are disabled and the
 * getVerifier()-method will return the null reference
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class Order extends GroupedParameterizedElement implements StrolchRootElement {

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
	public Order getClone() {
		Order clone = new Order();

		super.fillClone(clone);

		clone.setDate(this.date);
		clone.setState(this.state);

		return clone;
	}

	@Override
	protected void fillLocator(LocatorBuilder lb) {
		lb.append(Tags.ORDER).append(getType()).append(getId());
	}

	@Override
	public Locator getLocator() {
		LocatorBuilder lb = new LocatorBuilder();
		fillLocator(lb);
		return lb.build();
	}

	@Override
	public StrolchElement getParent() {
		return null;
	}

	@Override
	public Order getRootElement() {
		return this;
	}

	@Override
	public boolean isRootElement() {
		return true;
	}

	@Override
	public <T> T accept(StrolchRootElementVisitor<T> visitor) {
		return visitor.visitOrder(this);
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
