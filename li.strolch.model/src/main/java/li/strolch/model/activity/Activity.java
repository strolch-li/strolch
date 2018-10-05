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

import java.text.MessageFormat;
import java.util.*;
import java.util.Map.Entry;
import java.util.stream.Stream;

import li.strolch.exception.StrolchException;
import li.strolch.exception.StrolchModelException;
import li.strolch.exception.StrolchPolicyException;
import li.strolch.model.*;
import li.strolch.model.Locator.LocatorBuilder;
import li.strolch.model.parameter.Parameter;
import li.strolch.model.policy.PolicyDef;
import li.strolch.model.policy.PolicyDefs;
import li.strolch.model.visitor.StrolchElementVisitor;
import li.strolch.utils.dbc.DBC;

/**
 * Parameterized object grouping a collection of {@link Activity} and {@link Action} objects defining the process to be
 * scheduled
 *
 * @author Martin Smock <martin.smock@bluewin.ch>
 */
public class Activity extends AbstractStrolchRootElement
		implements IActivityElement, StrolchRootElement, Comparable<Activity> {

	protected Locator locator;
	protected Version version;

	protected Activity parent;
	protected TimeOrdering timeOrdering;
	protected Map<String, IActivityElement> elements;
	protected PolicyDefs policyDefs;

	/**
	 * Empty constructor - for marshalling only!
	 */
	public Activity() {
		super();
	}

	/**
	 * Default constructor
	 *
	 * @param id
	 * 		the id
	 * @param name
	 * 		the name
	 * @param type
	 * 		the type
	 */
	public Activity(String id, String name, String type, TimeOrdering timeOrdering) {
		super(id, name, type);
		this.timeOrdering = timeOrdering;
	}

	@Override
	public String getObjectType() {
		return Tags.ACTIVITY;
	}

	public TimeOrdering getTimeOrdering() {
		return this.timeOrdering;
	}

	public void setTimeOrdering(TimeOrdering timeOrdering) {
		assertNotReadonly();
		this.timeOrdering = timeOrdering;
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
		if (!isRootElement())
			throw new IllegalStateException("Can't set the version on non root of " + getLocator());

		if (version != null && !getLocator().equals(version.getLocator())) {
			String msg = "Illegal version as locator is not same: Element: {0} Version: {1}";
			throw new IllegalArgumentException(MessageFormat.format(msg, getLocator(), version));
		}

		this.version = version;
	}

	private void initElements() {
		if (this.elements == null) {
			// use a LinkedHashMap since we will iterate elements in the order
			// added and lookup elements by ID
			this.elements = new LinkedHashMap<>();
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
	 * 		the id of the element to check for
	 *
	 * @return true if this {@link Activity} contains a child with the given id. The element instance type is ignored,
	 * i.e. {@link Action} or {@link Activity}
	 */
	public boolean hasElement(String id) {
		return this.elements != null && this.elements.containsKey(id);
	}

	/**
	 * add an activity element to the <code>LinkedHashMap</code> of <code>IActivityElements</code>
	 *
	 * @param activityElement
	 * 		the element to add
	 *
	 * @return the element added
	 */
	@SuppressWarnings("unchecked")
	public <T extends IActivityElement> T addElement(IActivityElement activityElement) {
		assertNotReadonly();
		DBC.PRE.assertNotEquals("Can't add element to itself!", this, activityElement);
		DBC.PRE.assertNull("Parent can't already be set!", activityElement.getParent());

		// TODO make sure we can't create a circular dependency

		initElements();
		String id = activityElement.getId();
		if (id == null)
			throw new StrolchException("Cannot add IActivityElement without id.");
		else if (this.elements.containsKey(id))
			throw new StrolchException(
					"Activiy " + getLocator() + " already contains an activity element with id = " + id);
		else {
			activityElement.setParent(this);
			return (T) this.elements.put(activityElement.getId(), activityElement);
		}
	}

	/**
	 * Removes the element with the given id and returns it, if it exists
	 *
	 * @param id
	 * 		the id of the element to remove
	 *
	 * @return the removed element, or null if it does not exist
	 */
	@SuppressWarnings("unchecked")
	public <T extends IActivityElement> T remove(String id) {
		assertNotReadonly();
		IActivityElement element = this.elements.remove(id);
		if (element != null)
			element.setParent(null);
		return (T) element;
	}

	/**
	 * get <code>IActivityElement</code> by id
	 *
	 * @param id
	 * 		the id of the <code>IActivityElement</code>
	 *
	 * @return IActivityElement
	 */
	@SuppressWarnings("unchecked")
	public <T extends IActivityElement> T getElement(String id) {
		if (this.elements == null)
			return null;
		return (T) this.elements.get(id);
	}

	public Optional<IActivityElement> getPreviousElement(IActivityElement element) {
		if (!hasElements())
			return Optional.empty();

		IActivityElement previous = null;

		Iterator<Entry<String, IActivityElement>> iter = elementIterator();
		while (iter.hasNext()) {
			IActivityElement elem = iter.next().getValue();
			if (elem == element)
				return Optional.ofNullable(previous);
			else
				previous = elem;
		}

		return Optional.empty();
	}

	public Optional<IActivityElement> getPreviousElementByType(IActivityElement element, String type) {
		if (!hasElements())
			return Optional.empty();

		List<IActivityElement> reversed = new ArrayList<>(this.elements.values());
		Collections.reverse(reversed);

		boolean foundElem = false;
		Iterator<IActivityElement> iter = reversed.iterator();
		IActivityElement elem;
		while (iter.hasNext()) {
			elem = iter.next();

			if (foundElem && elem.getType().equals(type))
				return Optional.of(elem);
			else if (elem == element) {
				foundElem = true;
			}
		}

		return Optional.empty();
	}

	public Optional<IActivityElement> getNextElement(IActivityElement element) {
		if (!hasElements())
			return Optional.empty();

		Iterator<Entry<String, IActivityElement>> iter = elementIterator();
		IActivityElement previous = iter.next().getValue();

		while (iter.hasNext()) {
			IActivityElement elem = iter.next().getValue();
			if (previous == element)
				return Optional.ofNullable(elem);
			else
				previous = elem;
		}

		return Optional.empty();
	}

	public Optional<IActivityElement> getNextElementByType(IActivityElement element, String type) {
		if (!hasElements())
			return Optional.empty();

		Iterator<Entry<String, IActivityElement>> iter = elementIterator();

		boolean foundElem = false;
		IActivityElement elem;
		while (iter.hasNext()) {
			elem = iter.next().getValue();

			if (foundElem && elem.getType().equals(type))
				return Optional.of(elem);
			else if (elem == element) {
				foundElem = true;
			}
		}

		return Optional.empty();
	}

	public List<IActivityElement> getElementsByType(String type) {
		List<IActivityElement> elements = new ArrayList<>();
		Iterator<Entry<String, IActivityElement>> iter = elementIterator();
		while (iter.hasNext()) {
			IActivityElement element = iter.next().getValue();
			if (element.getType().equals(type))
				elements.add(element);
		}
		return elements;
	}

	/**
	 * @return get the <code>LinkedHashMap</code> of <code>IActivityElements</code>
	 */
	public Map<String, IActivityElement> getElements() {
		if (this.elements == null)
			return Collections.emptyMap();
		return this.elements;
	}

	/**
	 * Returns all the actions as a flat list
	 *
	 * @return the list of actions
	 */
	public List<Action> getActionsAsFlatList() {
		List<Action> actions = new ArrayList<>();
		getActionsAsFlatList(actions);
		return actions;
	}

	private void getActionsAsFlatList(List<Action> actions) {
		for (IActivityElement element : this.elements.values()) {
			if (element instanceof Activity)
				((Activity) element).getActionsAsFlatList(actions);
			else
				actions.add((Action) element);
		}
	}

	/**
	 * Returns all the actions in the entire hierarchy with the given state
	 *
	 * @param state
	 * 		the state of the action to return
	 *
	 * @return the list of actions with the given state
	 */
	public List<Action> getActionsWithState(State state) {
		List<Action> actions = new ArrayList<>();
		getActionsWithState(actions, state);
		return actions;
	}

	private void getActionsWithState(List<Action> actions, State state) {
		for (IActivityElement element : this.elements.values()) {
			if (element instanceof Activity)
				((Activity) element).getActionsWithState(actions, state);
			else if (element.getState() == state)
				actions.add((Action) element);
		}
	}

	/**
	 * Returns all the actions in the entire hierarchy with the given type
	 *
	 * @param type
	 * 		the type of action to return
	 *
	 * @return the list of actions with the given type
	 */
	public List<Action> getActionsByType(String type) {
		List<Action> actions = new ArrayList<>();
		getActionsByType(actions, type);
		return actions;
	}

	private void getActionsByType(List<Action> actions, String type) {
		for (IActivityElement element : this.elements.values()) {
			if (element instanceof Activity)
				((Activity) element).getActionsByType(actions, type);
			else if (element.getType().equals(type))
				actions.add((Action) element);
		}
	}

	/**
	 * @return the iterator for entries, which include the id as key and the {@link IActivityElement} as value
	 */
	public Iterator<Entry<String, IActivityElement>> elementIterator() {
		if (this.elements == null)
			return Collections.<String, IActivityElement>emptyMap().entrySet().iterator();
		return this.elements.entrySet().iterator();
	}

	/**
	 * @return the stream for entries, which include the id as key and the {@link IActivityElement} as value
	 */
	public Stream<Entry<String, IActivityElement>> elementStream() {
		if (this.elements == null)
			return Collections.<String, IActivityElement>emptyMap().entrySet().stream();
		return this.elements.entrySet().stream();
	}

	@Override
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

	@Override
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

	@Override
	public State getState() {
		if (this.elements == null || this.elements.isEmpty())
			return State.CREATED;
		return State.getState(this);
	}

	@Override
	public PolicyDefs getPolicyDefs() {
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
	public Locator getLocator() {
		if (this.locator == null) {
			LocatorBuilder lb = new LocatorBuilder();
			fillLocator(lb);
			this.locator = lb.build();
		}
		return this.locator;
	}

	@Override
	protected void fillLocator(LocatorBuilder locatorBuilder) {
		if (this.parent != null)
			this.parent.fillLocator(locatorBuilder);
		else
			locatorBuilder.append(Tags.ACTIVITY).append(getType());
		locatorBuilder.append(getId());
	}

	@Override
	public Activity getParent() {
		return this.parent;
	}

	@Override
	public Activity getRootElement() {
		return (this.parent == null) ? this : this.parent.getRootElement();
	}

	@Override
	public boolean isRootElement() {
		return (this.parent == null);
	}

	@Override
	public Activity getClone() {
		return getClone(false);
	}

	@Override
	public Activity getClone(boolean withVersion) {

		Activity clone = new Activity();
		clone.timeOrdering = this.timeOrdering;

		super.fillClone(clone);

		if (this.elements == null)
			return clone;

		for (IActivityElement element : this.elements.values()) {
			clone.addElement(element.getClone());
		}

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
		elementStream().forEach(e -> e.getValue().setReadOnly());
		super.setReadOnly();
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
		builder.append(getState());
		builder.append(", start=");
		builder.append(getStart());
		builder.append(", end=");
		builder.append(getEnd());
		if (isRootElement()) {
			builder.append(", version=");
			builder.append(this.version);
		}
		builder.append("]");
		return builder.toString();
	}

	@Override
	public int compareTo(Activity o) {
		return getId().compareTo(o.getId());
	}

	@Override
	public <T> T accept(StrolchElementVisitor<T> visitor) {
		return visitor.visitActivity(this);
	}

	@SuppressWarnings("unchecked")
	@Override
	public <U, T extends Parameter<U>> T findParameter(String bagKey, String paramKey) {

		T parameter = getParameter(bagKey, paramKey);
		if (parameter != null)
			return parameter;

		if (this.parent != null)
			return (T) this.parent.findParameter(bagKey, paramKey);

		return null;
	}

	@Override
	public <U, T extends Parameter<U>> T findParameter(String bagKey, String paramKey, boolean assertExists)
			throws StrolchModelException {

		T parameter = getParameter(bagKey, paramKey);
		if (parameter != null)
			return parameter;

		parameter = this.parent == null ? null : this.parent.findParameter(bagKey, paramKey);
		if (assertExists && parameter == null) {
			String msg = "The Parameter {0} does not exist";
			throw new StrolchModelException(MessageFormat.format(msg, getLocator().append(Tags.BAG, bagKey, paramKey)));
		}

		return parameter;
	}

	@Override
	public void setParent(Activity activity) {
		assertNotReadonly();
		this.parent = activity;
	}

	public static Locator locatorFor(String type, String id) {
		return Locator.valueOf(Tags.ACTIVITY, type, id);
	}
}
