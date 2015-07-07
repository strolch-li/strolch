/*
 * Copyright 2015 Martin Smock <martin.smock@bluewin.ch>
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
package li.strolch.model.activity;

import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;

import li.strolch.exception.StrolchException;
import li.strolch.model.GroupedParameterizedElement;
import li.strolch.model.Locator;
import li.strolch.model.Locator.LocatorBuilder;
import li.strolch.model.State;
import li.strolch.model.StrolchElement;
import li.strolch.model.StrolchRootElement;
import li.strolch.model.Tags;
import li.strolch.model.visitor.StrolchRootElementVisitor;
import ch.eitchnet.utils.dbc.DBC;

/**
 * Parameterized object grouping a collection of {@link Activity} and {@link Action} objects defining the process to be
 * scheduled
 * 
 * @author Martin Smock <martin.smock@bluewin.ch>
 */
public class Activity extends GroupedParameterizedElement implements IActivityElement, StrolchRootElement {

	private static final long serialVersionUID = 1L;

	protected Activity parent;

	protected Map<String, IActivityElement> elements;

	/**
	 * Empty constructor - for marshalling only!
	 */
	public Activity() {
		super();
	}

	public Activity(String id, String name, String type) {
		super(id, name, type);
	}

	private void initElements() {
		if (this.elements == null) {
			// use a LinkedHashMap since we will iterate elements in the order added and lookup elements by ID
			elements = new LinkedHashMap<String, IActivityElement>();
		}
	}

	/**
	 * Returns true if this {@link Activity} contains any children i.e. any of {@link Action} or {@link Activity}
	 * 
	 * @return true if this {@link Activity} contains any children i.e. any of {@link Action} or {@link Activity}
	 */
	public boolean hasElements() {
		return this.elements != null && !this.elements.isEmpty();
	}

	/**
	 * Returns true if this {@link Activity} contains a child with the given id. The element instance type is ignored,
	 * i.e. {@link Action} or {@link Activity}
	 * 
	 * @param id
	 *            the id of the element to check for
	 * 
	 * @return true if this {@link Activity} contains a child with the given id. The element instance type is ignored,
	 *         i.e. {@link Action} or {@link Activity}
	 */
	public boolean hasElement(String id) {
		return this.elements != null && this.elements.containsKey(id);
	}

	/**
	 * add an activity element to the <code>LinkedHashMap</code> of <code>IActivityElements</code>
	 * 
	 * @param activityElement
	 * @return the element added
	 */
	public IActivityElement addElement(IActivityElement activityElement) {
		DBC.PRE.assertNotEquals("Can't add element to itself!", this, activityElement);
		DBC.PRE.assertNull("Parent can't already be set!", activityElement.getParent());

		// TODO make sure we can't create a circular dependency

		initElements();
		String id = activityElement.getId();
		if (id == null)
			throw new StrolchException("Cannot add IActivityElement without id.");
		else if (elements.containsKey(id))
			throw new StrolchException("Activiy " + getLocator() + " already contains an activity element with id = "
					+ id);
		else {
			activityElement.setParent(this);
			return elements.put(activityElement.getId(), activityElement);
		}
	}

	/**
	 * get <code>IActivityElement</code> by id
	 * 
	 * @param id
	 *            the id of the <code>IActivityElement</code>
	 * @return IActivityElement
	 */
	@SuppressWarnings("unchecked")
	public <T extends IActivityElement> T getElement(String id) {
		if (this.elements == null)
			return null;
		return (T) elements.get(id);
	}

	/**
	 * @return get the <code>LinkedHashMap</code> of <code>IActivityElements</code>
	 */
	public Map<String, IActivityElement> getElements() {
		if (this.elements == null)
			return Collections.emptyMap();
		return elements;
	}

	/**
	 * @return the iterator for entries, which include the id as key and the {@link IActivityElement} as value
	 */
	public Iterator<Entry<String, IActivityElement>> elementIterator() {
		if (this.elements == null)
			return Collections.<String, IActivityElement> emptyMap().entrySet().iterator();
		return elements.entrySet().iterator();
	}

	public Long getStart() {
		Long start = Long.MAX_VALUE;
		if (this.elements == null)
			return start;
		Iterator<Entry<String, IActivityElement>> elementIterator = elementIterator();
		while (elementIterator.hasNext()) {
			IActivityElement action = elementIterator.next().getValue();
			start = Math.min(start, action.getStart());
		}
		return start;
	}

	public Long getEnd() {
		Long end = 0L;
		if (this.elements == null)
			return end;
		Iterator<Entry<String, IActivityElement>> elementIterator = elementIterator();
		while (elementIterator.hasNext()) {
			IActivityElement action = elementIterator.next().getValue();
			end = Math.max(end, action.getEnd());
		}
		return end;
	}

	public State getState() {
		State state = State.PLANNED;
		if (this.elements == null)
			return state;
		Iterator<Entry<String, IActivityElement>> elementIterator = elementIterator();
		while (elementIterator.hasNext()) {
			IActivityElement child = elementIterator.next().getValue();
			State childState = child.getState();
			if (childState.ordinal() < state.ordinal()) {
				state = childState;
			}
		}
		return state;
	}

	@Override
	public Locator getLocator() {
		LocatorBuilder lb = new LocatorBuilder();
		fillLocator(lb);
		return lb.build();
	}

	@Override
	protected void fillLocator(LocatorBuilder locatorBuilder) {
		locatorBuilder.append(Tags.ACTIVITY).append(getType()).append(getId());
	}

	@Override
	public StrolchElement getParent() {
		return parent;
	}

	@Override
	public StrolchRootElement getRootElement() {
		return (parent == null) ? this : parent.getRootElement();
	}

	@Override
	public boolean isRootElement() {
		return (parent == null);
	}

	@Override
	public Activity getClone() {
		Activity clone = new Activity();
		super.fillClone(clone);

		if (this.elements == null)
			return clone;

		for (IActivityElement element : this.elements.values()) {
			clone.addElement(element.getClone());
		}
		return clone;
	}

	@Override
	public String toString() {
		final StringBuilder builder = new StringBuilder();
		builder.append("Activity [id=");
		builder.append(this.id);
		builder.append(", name=");
		builder.append(this.name);
		builder.append(", type=");
		builder.append(this.type);
		builder.append(", state=");
		builder.append(this.getState());
		builder.append(", start=");
		builder.append(this.getStart());
		builder.append(", end=");
		builder.append(this.getEnd());
		builder.append("]");
		return builder.toString();
	}

	@Override
	public <T> T accept(StrolchRootElementVisitor<T> visitor) {
		throw new StrolchException("not implemented yet");
	}

	@Override
	public void setParent(Activity activity) {
		this.parent = activity;
	}
}
