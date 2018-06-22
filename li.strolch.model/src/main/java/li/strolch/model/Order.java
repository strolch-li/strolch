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

import java.text.MessageFormat;
import java.util.Date;

import li.strolch.exception.StrolchPolicyException;
import li.strolch.model.Locator.LocatorBuilder;
import li.strolch.model.policy.PolicyDef;
import li.strolch.model.policy.PolicyDefs;
import li.strolch.model.visitor.StrolchElementVisitor;
import li.strolch.utils.iso8601.ISO8601FormatFactory;

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
public class Order extends AbstractStrolchRootElement implements StrolchRootElement, Comparable<Order> {

	private static final long serialVersionUID = 0L;

	protected Locator locator;
	protected Version version;
	protected Date date;
	protected State state;
	protected PolicyDefs policyDefs;

	/**
	 * Empty constructor - for marshalling only!
	 */
	public Order() {
		super();
	}

	/**
	 * Default Constructor
	 *
	 * @param id
	 * 		the id
	 * @param name
	 * 		the name
	 * @param type
	 * 		the type
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
	 * 		the id
	 * @param name
	 * 		the name
	 * @param type
	 * 		the type
	 * @param date
	 * 		the date
	 * @param state
	 * 		the state
	 */
	public Order(String id, String name, String type, Date date, State state) {
		super(id, name, type);

		setState(state);
		setDate(date);
	}

	@Override
	public String getObjectType() {
		return Tags.ORDER;
	}

	@Override
	public boolean hasVersion() {
		return this.version != null;
	}

	@Override
	public Version getVersion() {
		return this.version;
	}

	@Override
	public void setVersion(Version version) throws IllegalArgumentException, IllegalStateException {
		if (version != null && !getLocator().equals(version.getLocator())) {
			String msg = "Illegal version as locator is not same: Element: {0} Version: {1}";
			throw new IllegalArgumentException(MessageFormat.format(msg, getLocator(), version));
		}

		this.version = version;
	}

	public Date getDate() {
		return this.date;
	}

	public void setDate(Date date) {
		assertNotReadonly();
		this.date = date;
	}

	public State getState() {
		return this.state;
	}

	public void setState(State state) {
		assertNotReadonly();
		this.state = state;
	}

	@Override
	public PolicyDefs getPolicyDefs() throws StrolchPolicyException {
		if (this.policyDefs == null)
			throw new StrolchPolicyException(getLocator() + " has no Policies defined!");
		return this.policyDefs;
	}

	@Override
	public PolicyDef getPolicyDef(Class<?> clazz) {
		return getPolicyDefs().getPolicyDef(clazz.getSimpleName());
	}

	@Override
	public PolicyDef getPolicyDef(String type) {
		return getPolicyDefs().getPolicyDef(type);
	}

	@Override
	public boolean hasPolicyDefs() {
		return this.policyDefs != null;
	}

	@Override
	public boolean hasPolicyDef(String type) {
		return this.policyDefs != null && policyDefs.hasPolicyDef(type);
	}

	@Override
	public void setPolicyDefs(PolicyDefs policyDefs) {
		assertNotReadonly();
		this.policyDefs = policyDefs;
		this.policyDefs.setParent(this);
	}

	@Override
	public Order getClone() {
		return getClone(false);
	}

	@Override
	public Order getClone(boolean withVersion) {

		Order clone = new Order();

		super.fillClone(clone);

		clone.setDate(this.date);
		clone.setState(this.state);

		if (this.policyDefs != null)
			clone.setPolicyDefs(this.policyDefs.getClone());

		if (withVersion)
			clone.setVersion(this.version);

		return clone;
	}

	@Override
	public void setReadOnly() {
		if (this.policyDefs != null)
			this.policyDefs.setReadOnly();
		super.setReadOnly();
	}

	@Override
	protected void fillLocator(LocatorBuilder lb) {
		lb.append(Tags.ORDER).append(getType()).append(getId());
	}

	@Override
	public Locator getLocator() {
		if (this.locator == null) {
			LocatorBuilder lb = new LocatorBuilder();
			fillLocator(lb);
			this.locator = lb.build();
		}
		return this.locator;
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
	public <T> T accept(StrolchElementVisitor<T> visitor) {
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
		builder.append(", version=");
		builder.append(this.version);
		builder.append("]");

		return builder.toString();
	}

	@Override
	public int compareTo(Order o) {
		return getId().compareTo(o.getId());
	}

	public static Locator locatorFor(String type, String id) {
		return Locator.valueOf(Tags.ORDER, type, id);
	}
}
