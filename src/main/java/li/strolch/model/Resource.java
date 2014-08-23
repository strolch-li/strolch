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
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import li.strolch.exception.StrolchException;
import li.strolch.model.Locator.LocatorBuilder;
import li.strolch.model.timedstate.BooleanTimedState;
import li.strolch.model.timedstate.FloatTimedState;
import li.strolch.model.timedstate.IntegerTimedState;
import li.strolch.model.timedstate.StringSetTimedState;
import li.strolch.model.timedstate.StrolchTimedState;
import li.strolch.model.timevalue.IValue;
import li.strolch.model.visitor.StrolchRootElementVisitor;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import ch.eitchnet.utils.dbc.DBC;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class Resource extends GroupedParameterizedElement implements StrolchRootElement {

	private static final long serialVersionUID = 0L;

	private Map<String, StrolchTimedState<IValue<?>>> timedStateMap;

	/**
	 * Empty constructor
	 */
	public Resource() {
		//
	}

	/**
	 * Default constructor
	 *
	 * @param id
	 * @param name
	 * @param type
	 */
	public Resource(String id, String name, String type) {
		super(id, name, type);
	}

	/**
	 * DOM Constructor
	 *
	 * @param element
	 */
	public Resource(Element element) {
		super.fromDom(element);

		NodeList timedStateElems = element.getElementsByTagName(Tags.TIMED_STATE);
		for (int i = 0; i < timedStateElems.getLength(); i++) {
			Element timedStateElem = (Element) timedStateElems.item(i);
			String typeS = timedStateElem.getAttribute(Tags.TYPE);

			DBC.PRE.assertNotEmpty("Type must be set on TimedState for resource with id " + id, typeS);

			if (typeS.equals(FloatTimedState.TYPE)) {
				FloatTimedState timedState = new FloatTimedState(timedStateElem);
				addTimedState(timedState);
			} else if (typeS.equals(IntegerTimedState.TYPE)) {
				IntegerTimedState timedState = new IntegerTimedState(timedStateElem);
				addTimedState(timedState);
			} else if (typeS.equals(BooleanTimedState.TYPE)) {
				BooleanTimedState timedState = new BooleanTimedState(timedStateElem);
				addTimedState(timedState);
			} else if (typeS.equals(StringSetTimedState.TYPE)) {
				StringSetTimedState timedState = new StringSetTimedState(timedStateElem);
				addTimedState(timedState);
			} else {
				String msg = "What kind of TimedState is this: {0}"; //$NON-NLS-1$
				msg = MessageFormat.format(msg, typeS);
				throw new StrolchException(msg);
			}
		}
	}

	@SuppressWarnings("unchecked")
	public void addTimedState(StrolchTimedState<?> strolchTimedState) {
		if (this.timedStateMap == null) {
			this.timedStateMap = new HashMap<>();
		}

		this.timedStateMap.put(strolchTimedState.getId(), (StrolchTimedState<IValue<?>>) strolchTimedState);
		strolchTimedState.setParent(this);
	}

	@SuppressWarnings({ "rawtypes", "unchecked" })
	public <T extends StrolchTimedState> T getTimedState(String id) {
		if (this.timedStateMap == null) {
			return null;
		}
		return (T) this.timedStateMap.get(id);
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	public <T extends StrolchTimedState> T removeTimedState(String id) {
		if (this.timedStateMap == null) {
			return null;
		}
		return (T) this.timedStateMap.remove(id);
	}

	public Set<String> getTimedStateKeySet() {
		if (this.timedStateMap == null) {
			return Collections.emptySet();
		}
		return new HashSet<>(this.timedStateMap.keySet());
	}

	public List<StrolchTimedState<IValue<?>>> getTimedStates() {
		if (this.timedStateMap == null) {
			return Collections.emptyList();
		}
		return new ArrayList<>(this.timedStateMap.values());
	}

	public boolean hasTimedStates() {
		return this.timedStateMap != null && !this.timedStateMap.isEmpty();
	}

	public boolean hasTimedState(String id) {
		return this.timedStateMap != null && this.timedStateMap.containsKey(id);
	}

	@Override
	public Element toDom(Document doc) {

		Element element = doc.createElement(Tags.RESOURCE);
		fillElement(element);

		if (this.timedStateMap != null) {
			for (StrolchTimedState<?> state : this.timedStateMap.values()) {
				Element timedStateElem = state.toDom(element.getOwnerDocument());
				element.appendChild(timedStateElem);
			}
		}

		return element;
	}

	@Override
	public Resource getClone() {
		Resource clone = new Resource();

		super.fillClone(clone);

		return clone;
	}

	@Override
	public void fillLocator(LocatorBuilder lb) {
		lb.append(Tags.RESOURCE).append(getType()).append(getId());
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
	public Resource getRootElement() {
		return this;
	}

	@Override
	public <T> T accept(StrolchRootElementVisitor<T> visitor) {
		return visitor.visitResource(this);
	}

	@SuppressWarnings("nls")
	@Override
	public String toString() {

		StringBuilder builder = new StringBuilder();

		builder.append("Resource [id=");
		builder.append(this.id);
		builder.append(", name=");
		builder.append(this.name);
		builder.append(", type=");
		builder.append(this.type);
		builder.append("]");

		return builder.toString();
	}
}
