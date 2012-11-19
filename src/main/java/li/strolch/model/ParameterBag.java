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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import li.strolch.exception.StrolchException;
import li.strolch.model.Locator.LocatorBuilder;

import org.dom4j.Element;
import org.dom4j.tree.DefaultElement;

import ch.eitchnet.utils.helper.StringHelper;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public class ParameterBag extends AbstractStrolchElement {

	private static final long serialVersionUID = 0L;

	protected ParameterizedElement parent;
	protected Map<String, Parameter<?>> parameterMap;
	protected String type;

	/**
	 * Empty constructor
	 */
	public ParameterBag() {
		
		this.parameterMap = new HashMap<String, Parameter<?>>();
	}

	/**
	 * @param id
	 * @param name
	 * @param type
	 */
	public ParameterBag(String id, String name, String type) {
		setId(id);
		setName(name);
		setType(type);

		this.parameterMap = new HashMap<String, Parameter<?>>();
	}

	/**
	 * @param element
	 */
	public ParameterBag(Element element) {
		super.fromDom(element);

		String type = element.attributeValue("Type");
		setType(type);

		this.parameterMap = new HashMap<String, Parameter<?>>();

		// add all the parameters
		@SuppressWarnings("unchecked")
		List<Element> parameterElements = element.elements("Parameter");
		for (Object object : parameterElements) {
			Element paramElement = (Element) object;
			String paramtype = paramElement.attributeValue("Type");

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
			} else {
				throw new StrolchException("What kind of parameter is this: " + paramtype);
			}
		}
	}

	@Override
	public String getType() {
		return this.type;
	}

	/**
	 * Sets the type of this {@link ParameterBag}
	 * 
	 * @param type
	 *            the type to set
	 */
	public void setType(String type) {
		if (StringHelper.isEmpty(type))
			throw new StrolchException("Type must be set on element " + getLocator());

		this.type = type;
	}

	/**
	 * Returns this {@link ParameterBag}'s parent
	 * 
	 * @return the parent
	 */
	public ParameterizedElement getParent() {
		return this.parent;
	}

	/**
	 * Set the parent for this {@link ParameterBag}
	 * 
	 * @param parent
	 *            the parent to set
	 */
	public void setParent(ParameterizedElement parent) {
		this.parent = parent;
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
		return (T) this.parameterMap.get(key);
	}

	/**
	 * Adds the given {@link Parameter} to the {@link ParameterBag}
	 * 
	 * @param parameter
	 *            the {@link Parameter} to add
	 */
	public void addParameter(Parameter<?> parameter) {
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
		return (Parameter<T>) this.parameterMap.remove(key);
	}

	/**
	 * Returns a list of all the {@link Parameter}s in this {@link ParameterBag}
	 * 
	 * @return a list of all the {@link Parameter}s in this {@link ParameterBag}
	 */
	public List<Parameter<?>> getParameters() {
		return new ArrayList<Parameter<?>>(this.parameterMap.values());
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
		return this.parameterMap.containsKey(key);
	}

	/**
	 * Returns a {@link Set} of all the {@link Parameter} keys in this {@link ParameterBag}
	 * 
	 * @return a {@link Set} of all the {@link Parameter} keys in this {@link ParameterBag}
	 */
	public Set<String> getParameterKeySet() {
		return new HashSet<String>(this.parameterMap.keySet());
	}

	@Override
	protected void fillLocator(LocatorBuilder lb) {
		lb.append("ParameterBag").append(this.id);
	}

	@Override
	public Locator getLocator() {
		LocatorBuilder lb = new LocatorBuilder();
		this.parent.fillLocator(lb);
		fillLocator(lb);
		return lb.build();
	}

	@Override
	public Element toDom() {

		Element element = new DefaultElement("ParameterBag");
		fillElement(element);

		for (Parameter<?> parameter : this.parameterMap.values()) {
			element.add(parameter.toDom());
		}

		return element;
	}

	@Override
	public ParameterBag getClone() {
		ParameterBag clone = new ParameterBag();

		super.fillClone(clone);

		clone.setType(this.type);

		return clone;
	}

	@Override
	public String toString() {

		StringBuilder builder = new StringBuilder();

		builder.append("ParameterBag [id=");
		builder.append(this.id);
		builder.append(", name=");
		builder.append(this.name);
		builder.append(", type=");
		builder.append(this.type);
		builder.append("]");

		return builder.toString();
	}
}
