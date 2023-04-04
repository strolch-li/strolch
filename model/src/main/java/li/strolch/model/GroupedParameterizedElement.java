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

import static java.util.Collections.emptySet;
import static li.strolch.model.StrolchModelConstants.BAG_PARAMETERS;
import static li.strolch.model.StrolchModelConstants.BAG_RELATIONS;
import static li.strolch.utils.helper.StringHelper.isEmpty;

import java.text.MessageFormat;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.stream.Stream;

import li.strolch.exception.StrolchException;
import li.strolch.exception.StrolchModelException;
import li.strolch.model.parameter.Parameter;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public abstract class GroupedParameterizedElement extends AbstractStrolchElement implements ParameterBagContainer {

	protected Map<String, ParameterBag> parameterBagMap;
	protected String type;

	/**
	 * Empty constructor - for marshalling only!
	 */
	protected GroupedParameterizedElement() {
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
	protected GroupedParameterizedElement(String id, String name, String type) {
		super(id, name);
		setType(type);
	}

	@Override
	public String getType() {
		return this.type;
	}

	/**
	 * Sets the type of this {@link GroupedParameterizedElement}
	 *
	 * @param type
	 * 		the type to set
	 */
	public void setType(String type) {
		assertNotReadonly();
		if (isEmpty(type)) {
			String msg = "Type may not be empty on element {0}";
			msg = MessageFormat.format(msg, getLocator());
			throw new StrolchException(msg);
		}

		this.type = type.intern();
	}

	@Override
	public <U, T extends Parameter<U>> T getParameter(String bagKey, String paramKey) {
		return getParameter(bagKey, paramKey, false);
	}

	@Override
	public <U, T extends Parameter<U>> T getParameter(String bagKey, String paramKey, boolean assertExists)
			throws StrolchModelException {
		if (this.parameterBagMap == null) {
			if (assertExists) {
				String msg = "The Parameter {0} does not exist";
				throw new StrolchModelException(
						MessageFormat.format(msg, getLocator().append(Tags.BAG, bagKey, paramKey)));
			}

			return null;
		}
		ParameterBag bag = this.parameterBagMap.get(bagKey);
		if (bag == null) {
			if (assertExists) {
				String msg = "The Parameter {0} does not exist";
				throw new StrolchModelException(
						MessageFormat.format(msg, getLocator().append(Tags.BAG, bagKey, paramKey)));
			}

			return null;
		}

		T parameter = bag.getParameter(paramKey);
		if (assertExists && parameter == null) {
			String msg = "The Parameter {0} does not exist";
			throw new StrolchModelException(MessageFormat.format(msg, getLocator().append(Tags.BAG, bagKey, paramKey)));
		}
		return parameter;
	}

	@Override
	public <U, T extends Parameter<U>> T getParameter(String paramKey) {
		return getParameter(BAG_PARAMETERS, paramKey, false);
	}

	@Override
	public <U, T extends Parameter<U>> T getParameter(String paramKey, boolean assertExists) {
		return getParameter(BAG_PARAMETERS, paramKey, assertExists);
	}

	@Override
	public void addParameter(Parameter<?> parameter) throws StrolchException {
		assertNotReadonly();
		if (isEmpty(parameter.getId()))
			throw new IllegalArgumentException("ID can not be empty for parameter " + parameter);
		if (this.parameterBagMap == null)
			this.parameterBagMap = new HashMap<>(1, 1.0F);
		ParameterBag bag = this.parameterBagMap.get(BAG_PARAMETERS);
		if (bag == null) {
			String msg = "No parameter bag exists with key {0}";
			msg = MessageFormat.format(msg, BAG_PARAMETERS);
			throw new StrolchException(msg);
		}

		bag.addParameter(parameter);
	}

	@Override
	public void addParameter(String bagKey, Parameter<?> parameter) throws StrolchException {
		assertNotReadonly();
		if (this.parameterBagMap == null)
			this.parameterBagMap = new HashMap<>(1, 1.0F);
		ParameterBag bag = this.parameterBagMap.get(bagKey);
		if (bag == null) {
			String msg = "No parameter bag exists with key {0}";
			msg = MessageFormat.format(msg, bagKey);
			throw new StrolchException(msg);
		}

		bag.addParameter(parameter);
	}

	@Override
	public <U, T extends Parameter<U>> T removeParameter(String paramKey) {
		assertNotReadonly();
		if (this.parameterBagMap == null)
			return null;
		ParameterBag bag = this.parameterBagMap.get(BAG_PARAMETERS);
		return bag == null ? null : bag.removeParameter(paramKey);
	}

	@Override
	public <U, T extends Parameter<U>> T removeRelation(String paramKey) {
		assertNotReadonly();
		if (this.parameterBagMap == null)
			return null;
		ParameterBag bag = this.parameterBagMap.get(BAG_RELATIONS);
		return bag == null ? null : bag.removeParameter(paramKey);
	}

	@Override
	public <U, T extends Parameter<U>> T removeParameter(String bagKey, String paramKey) {
		assertNotReadonly();
		if (this.parameterBagMap == null)
			return null;
		ParameterBag bag = this.parameterBagMap.get(bagKey);
		return bag == null ? null : bag.removeParameter(paramKey);
	}

	@Override
	public ParameterBag getParameterBag(String key) {
		return getParameterBag(key, false);
	}

	@Override
	public ParameterBag getParameterBag(String key, boolean assertExists) {
		if (this.parameterBagMap == null) {
			if (assertExists) {
				String msg = "The ParameterBag {0} does not exist";
				throw new StrolchModelException(MessageFormat.format(msg, getLocator().append(Tags.BAG, key)));
			}
			return null;
		}

		ParameterBag parameterBag = this.parameterBagMap.get(key);
		if (assertExists && parameterBag == null) {
			String msg = "The ParameterBag {0} does not exist";
			throw new StrolchModelException(MessageFormat.format(msg, getLocator().append(Tags.BAG, key)));
		}

		return parameterBag;
	}

	@Override
	public Stream<ParameterBag> streamOfParameterBags() {
		if (this.parameterBagMap == null || this.parameterBagMap.isEmpty())
			return Stream.empty();
		return this.parameterBagMap.values().stream();
	}

	@Override
	public Stream<ParameterBag> streamOfParameterBagsByType(String type) {
		if (this.parameterBagMap == null || this.parameterBagMap.isEmpty())
			return Stream.empty();
		return this.parameterBagMap.values() //
				.stream() //
				.filter(map -> map.getType().equals(type));
	}

	@Override
	public void addParameterBag(ParameterBag bag) {
		assertNotReadonly();
		if (this.parameterBagMap == null)
			this.parameterBagMap = new HashMap<>(1, 1.0F);

		if (this.parameterBagMap.containsKey(bag.getId())) {
			String msg = "A ParameterBag already exists with id {0} on {1}";
			throw new StrolchException(MessageFormat.format(msg, bag.getId(), getLocator()));
		}
		this.parameterBagMap.put(bag.getId(), bag);
		bag.setParent(this);
	}

	@Override
	public ParameterBag removeParameterBag(String key) {
		assertNotReadonly();
		if (this.parameterBagMap == null)
			return null;
		return this.parameterBagMap.remove(key);
	}

	@Override
	public boolean hasParameterBags() {
		return this.parameterBagMap != null && !this.parameterBagMap.isEmpty();
	}

	@Override
	public boolean hasParameterBag(String bagKey) {
		return this.parameterBagMap != null && this.parameterBagMap.containsKey(bagKey);
	}

	@Override
	public boolean hasParameter(String bagKey, String paramKey) {
		if (this.parameterBagMap == null)
			return false;
		ParameterBag bag = this.parameterBagMap.get(bagKey);
		return bag != null && bag.hasParameter(paramKey);
	}

	@Override
	public Set<String> getParameterBagKeySet() {
		return this.parameterBagMap == null ? emptySet() : new HashSet<>(this.parameterBagMap.keySet());
	}

	/**
	 * Fills {@link GroupedParameterizedElement} properties of this clone
	 *
	 * @param clone
	 * 		the clone to fill
	 */
	protected void fillClone(GroupedParameterizedElement clone) {
		super.fillClone(clone);
		clone.type = this.type;

		if (this.parameterBagMap != null) {
			for (ParameterBag bag : this.parameterBagMap.values())
				clone.addParameterBag(bag.getClone());
		}
	}

	@Override
	public void setReadOnly() {
		if (this.parameterBagMap != null) {
			for (ParameterBag bag : this.parameterBagMap.values())
				bag.setReadOnly();
		}
		super.setReadOnly();
	}
}
