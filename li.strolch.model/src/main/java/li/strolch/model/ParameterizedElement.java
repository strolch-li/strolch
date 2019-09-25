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

import static java.util.stream.Collectors.toList;
import static java.util.stream.Collectors.toMap;

import java.text.MessageFormat;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Stream;

import li.strolch.exception.StrolchException;
import li.strolch.exception.StrolchModelException;
import li.strolch.model.Locator.LocatorBuilder;
import li.strolch.model.parameter.Parameter;
import li.strolch.utils.helper.StringHelper;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public abstract class ParameterizedElement extends AbstractStrolchElement {

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
	 * 		the id
	 * @param name
	 * 		the name
	 * @param type
	 * 		the type
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
	 * 		the type to set
	 */
	public void setType(String type) {
		assertNotReadonly();
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
	 * 		the id of the parameter to return
	 *
	 * @return the {@link Parameter} with the given id, or null if it does not exist
	 */
	public <U, T extends Parameter<U>> T getParameter(String key) {
		return getParameter(key, false);
	}

	/**
	 * Returns the {@link Parameter} with the given id, or null if it does not exist
	 *
	 * @param key
	 * 		the id of the parameter to return
	 * @param assertExists
	 * 		if set to true, and the parameter does not exist, a {@link StrolchModelException} is thrown
	 *
	 * @return the {@link Parameter} with the given id, or null if it does not exist
	 */
	@SuppressWarnings("unchecked")
	public <U, T extends Parameter<U>> T getParameter(String key, boolean assertExists) {
		if (this.parameterMap == null) {

			if (assertExists) {
				String msg = "The Parameter {0} does not exist";
				throw new StrolchModelException(MessageFormat.format(msg, getLocator().append(key)));
			}

			return null;
		}

		Parameter<?> parameter = this.parameterMap.get(key);
		if (assertExists && parameter == null) {
			String msg = "The Parameter {0} does not exist";
			throw new StrolchModelException(MessageFormat.format(msg, getLocator().append(key)));
		}

		return (T) parameter;
	}

	/**
	 * Adds the given {@link Parameter} to the {@link ParameterizedElement}
	 *
	 * @param parameter
	 * 		the {@link Parameter} to add
	 */
	public void addParameter(Parameter<?> parameter) {
		assertNotReadonly();
		if (this.parameterMap == null)
			this.parameterMap = new HashMap<>(1, 1.0F);

		if (this.parameterMap.containsKey(parameter.getId())) {
			String msg = "A Parameter already exists with id {0} on {1}";
			throw new StrolchException(MessageFormat.format(msg, parameter.getId(), getLocator()));
		}
		this.parameterMap.put(parameter.getId(), parameter);
		parameter.setParent(this);
	}

	/**
	 * Removes the {@link Parameter} with the given key
	 *
	 * @param key
	 * 		the key of the {@link Parameter} to remove
	 *
	 * @return the removed {@link Parameter}, or null if it does not exist
	 */
	@SuppressWarnings("unchecked")
	public <U, T extends Parameter<U>> T removeParameter(String key) {
		assertNotReadonly();
		if (this.parameterMap == null)
			return null;
		return (T) this.parameterMap.remove(key);
	}

	/**
	 * Returns a list of all the {@link Parameter Parameter} in this {@link ParameterizedElement}
	 *
	 * @return a list of all the {@link Parameter Parameter} in this {@link ParameterizedElement}
	 */
	public List<Parameter<?>> getParameters() {
		if (this.parameterMap == null)
			return Collections.emptyList();
		return new ArrayList<>(this.parameterMap.values());
	}

	/**
	 * Returns a {@link Stream} of all the {@link Parameter Parameters}
	 *
	 * @return the parameters
	 */
	public Stream<Parameter<?>> streamOfParameters() {
		if (this.parameterMap == null)
			return Stream.empty();

		return this.parameterMap.values().stream();
	}

	/**
	 * Returns a {@link Stream} of all the {@link Parameter Parameters} with the given interpretation
	 *
	 * @param interpretation
	 * 		the interpretation for which the parameters are to be returned
	 *
	 * @return the parameters with the given interpretation
	 */
	public Stream<Parameter<?>> streamOfParametersByInterpretation(String interpretation) {
		if (this.parameterMap == null)
			return Stream.empty();

		return this.parameterMap.values().stream().filter(p -> p.getInterpretation().equals(interpretation));
	}

	/**
	 * Returns a {@link Stream} of all the {@link Parameter Parameters} with the given interpretation
	 *
	 * @param interpretation
	 * 		the interpretation for which the parameters are to be returned
	 * @param uom
	 * 		the uom for which the parameters are to be returned
	 *
	 * @return the parameters with the given interpretation
	 */
	public Stream<Parameter<?>> streamOfParametersByInterpretationAndUom(String interpretation, String uom) {
		if (this.parameterMap == null)
			return Stream.empty();

		return this.parameterMap.values().stream()
				.filter(p -> p.getInterpretation().equals(interpretation) && p.getUom().equals(uom));
	}

	/**
	 * Returns a list of all the {@link Parameter Parameters} with the given interpretation
	 *
	 * @param interpretation
	 * 		the interpretation for which the parameters are to be returned
	 *
	 * @return the parameters with the given interpretation
	 */
	public List<Parameter<?>> getParametersByInterpretation(String interpretation) {
		return streamOfParametersByInterpretation(interpretation).collect(toList());
	}

	/**
	 * Returns a list of all the {@link Parameter Parameters} with the given interpretation
	 *
	 * @param interpretation
	 * 		the interpretation for which the parameters are to be returned
	 * @param uom
	 * 		the uom for which the parameters are to be returned
	 *
	 * @return the parameters with the given interpretation
	 */
	public List<Parameter<?>> getParametersByInterpretationAndUom(String interpretation, String uom) {
		return streamOfParametersByInterpretationAndUom(interpretation, uom).collect(toList());
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
	 * 		the key of the {@link Parameter} to check for
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
		return new HashSet<>(this.parameterMap.keySet());
	}

	/**
	 * Returns a simple map where the keys are the IDs of the parameters and the values are the values of the
	 * Parameters
	 *
	 * @return a simple map where the keys are the IDs of the parameters and the values are the values of the *
	 * Parameters
	 */
	public Map<String, Object> toObjectMap() {
		return this.parameterMap.values().stream()
				.collect(toMap(StrolchElement::getId, (Function<Parameter<?>, Object>) Parameter::getValue));
	}

	@Override
	public abstract void fillLocator(LocatorBuilder lb);

	@Override
	public Locator getLocator() {
		LocatorBuilder lb = new LocatorBuilder();
		fillLocator(lb);
		return lb.build();
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
	public void setReadOnly() {
		if (this.parameterMap != null) {
			for (Parameter<?> param : this.parameterMap.values()) {
				param.setReadOnly();
			}
		}
		super.setReadOnly();
	}

	@Override
	public GroupedParameterizedElement getParent() {
		return this.parent;
	}

	/**
	 * Set the parent for this {@link ParameterizedElement}
	 *
	 * @param parent
	 * 		the parent to set
	 */
	public void setParent(GroupedParameterizedElement parent) {
		assertNotReadonly();
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
