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
import li.strolch.model.parameter.BooleanParameter;
import li.strolch.model.parameter.DateParameter;
import li.strolch.model.parameter.FloatParameter;
import li.strolch.model.parameter.IntegerParameter;
import li.strolch.model.parameter.LongParameter;
import li.strolch.model.parameter.Parameter;
import li.strolch.model.parameter.StringListParameter;
import li.strolch.model.parameter.StringParameter;

import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import ch.eitchnet.utils.helper.StringHelper;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public abstract class ParameterizedElement extends AbstractStrolchElement {

	private static final long serialVersionUID = 0L;

	protected GroupedParameterizedElement parent;
	protected Map<String, Parameter<?>> parameterMap;
	protected String type;

	/**
	 * Empty Constructor
	 */
	protected ParameterizedElement() {
		//
	}

	/**
	 * Default Constructor
	 * 
	 * @param id
	 * @param name
	 * @param type
	 */
	public ParameterizedElement(String id, String name, String type) {
		setId(id);
		setName(name);
		setType(type);
	}

	@Override
	public String getType() {
		return this.type;
	}

	/**
	 * Sets the type of this {@link ParameterizedElement}
	 * 
	 * @param type
	 *            the type to set
	 */
	public void setType(String type) {
		if (StringHelper.isEmpty(type)) {
			String msg = "Type may not be empty on element {0}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, getLocator());
			throw new StrolchException(msg);
		}

		this.type = type;
	}

	/**
	 * Returns the {@link Parameter} with the given id, or null if it does not exist
	 * 
	 * @param key
	 *            the id of the parameter to return
	 * 
	 * @return the {@link Parameter} with the given id, or null if it does not exist
	 */
	@SuppressWarnings("unchecked")
	public <T> T getParameter(String key) {
		if (this.parameterMap == null)
			return null;
		return (T) this.parameterMap.get(key);
	}

	/**
	 * Adds the given {@link Parameter} to the {@link ParameterizedElement}
	 * 
	 * @param parameter
	 *            the {@link Parameter} to add
	 */
	public void addParameter(Parameter<?> parameter) {
		if (this.parameterMap == null)
			this.parameterMap = new HashMap<String, Parameter<?>>();
		this.parameterMap.put(parameter.getId(), parameter);
		parameter.setParent(this);
	}

	/**
	 * Removes the {@link Parameter} with the given key
	 * 
	 * @param key
	 *            the key of the {@link Parameter} to remove
	 * 
	 * @return the removed {@link Parameter}, or null if it does not exist
	 */
	@SuppressWarnings("unchecked")
	public <T> Parameter<T> removeParameter(String key) {
		if (this.parameterMap == null)
			return null;
		return (Parameter<T>) this.parameterMap.remove(key);
	}

	/**
	 * Returns a list of all the {@link Parameter}s in this {@link ParameterizedElement}
	 * 
	 * @return a list of all the {@link Parameter}s in this {@link ParameterizedElement}
	 */
	public List<Parameter<?>> getParameters() {
		if (this.parameterMap == null)
			return Collections.emptyList();
		return new ArrayList<Parameter<?>>(this.parameterMap.values());
	}

	/**
	 * Returns true, if the this {@link ParameterizedElement} has any {@link Parameter Parameters}, false otherwise
	 * 
	 * @return true, if the this {@link ParameterizedElement} has any {@link Parameter Parameters}, false otherwise
	 */
	public boolean hasParameters() {
		return this.parameterMap != null && !this.parameterMap.isEmpty();
	}

	/**
	 * Returns true, if the {@link Parameter} exists with the given key, false otherwise
	 * 
	 * @param key
	 *            the key of the {@link Parameter} to check for
	 * 
	 * @return true, if the {@link Parameter} exists with the given key, false otherwise
	 */
	public boolean hasParameter(String key) {
		if (this.parameterMap == null)
			return false;
		return this.parameterMap.containsKey(key);
	}

	/**
	 * Returns a {@link Set} of all the {@link Parameter} keys in this {@link ParameterizedElement}
	 * 
	 * @return a {@link Set} of all the {@link Parameter} keys in this {@link ParameterizedElement}
	 */
	public Set<String> getParameterKeySet() {
		if (this.parameterMap == null)
			return Collections.emptySet();
		return new HashSet<String>(this.parameterMap.keySet());
	}

	@Override
	public void fillLocator(LocatorBuilder lb) {
		this.parent.fillLocator(lb);
		lb.append(this.id);
	}

	@Override
	public Locator getLocator() {
		LocatorBuilder lb = new LocatorBuilder();
		this.parent.fillLocator(lb);
		fillLocator(lb);
		return lb.build();
	}

	@Override
	protected void fromDom(Element element) {
		super.fromDom(element);

		String type = element.getAttribute(Tags.TYPE);
		setType(type);

		// add all the parameters
		NodeList parameterElements = element.getElementsByTagName(Tags.PARAMETER);
		for (int i = 0; i < parameterElements.getLength(); i++) {
			Element paramElement = (Element) parameterElements.item(i);
			String paramtype = paramElement.getAttribute(Tags.TYPE);

			if (paramtype.equals(StringParameter.TYPE)) {
				StringParameter param = new StringParameter(paramElement);
				addParameter(param);
			} else if (paramtype.equals(IntegerParameter.TYPE)) {
				IntegerParameter param = new IntegerParameter(paramElement);
				addParameter(param);
			} else if (paramtype.equals(FloatParameter.TYPE)) {
				FloatParameter param = new FloatParameter(paramElement);
				addParameter(param);
			} else if (paramtype.equals(LongParameter.TYPE)) {
				LongParameter param = new LongParameter(paramElement);
				addParameter(param);
			} else if (paramtype.equals(DateParameter.TYPE)) {
				DateParameter param = new DateParameter(paramElement);
				addParameter(param);
			} else if (paramtype.equals(BooleanParameter.TYPE)) {
				BooleanParameter param = new BooleanParameter(paramElement);
				addParameter(param);
			} else if (paramtype.equals(StringListParameter.TYPE)) {
				StringListParameter param = new StringListParameter(paramElement);
				addParameter(param);
			} else {
				String msg = "What kind of parameter is this: {0}"; //$NON-NLS-1$
				msg = MessageFormat.format(msg, paramtype);
				throw new StrolchException(msg);
			}
		}
	}

	@Override
	protected void fillElement(Element element) {
		super.fillElement(element);

		if (this.parameterMap != null) {
			for (Parameter<?> parameter : this.parameterMap.values()) {
				element.appendChild(parameter.toDom(element.getOwnerDocument()));
			}
		}
	}

	@Override
	protected void fillClone(StrolchElement clone) {
		super.fillClone(clone);
		ParameterizedElement peClone = (ParameterizedElement) clone;
		peClone.setType(this.type);
		if (this.parameterMap != null) {
			for (Parameter<?> param : this.parameterMap.values()) {
				peClone.addParameter(param.getClone());
			}
		}
	}

	@Override
	public GroupedParameterizedElement getParent() {
		return this.parent;
	}

	/**
	 * Set the parent for this {@link ParameterizedElement}
	 * 
	 * @param parent
	 *            the parent to set
	 */
	public void setParent(GroupedParameterizedElement parent) {
		this.parent = parent;
	}

	@Override
	public StrolchRootElement getRootElement() {
		return this.parent.getRootElement();
	}

	@SuppressWarnings("nls")
	@Override
	public String toString() {

		StringBuilder builder = new StringBuilder();

		builder.append("ParameterizedElement [id=");
		builder.append(this.id);
		builder.append(", name=");
		builder.append(this.name);
		builder.append(", type=");
		builder.append(this.type);
		builder.append("]");

		return builder.toString();
	}
}
