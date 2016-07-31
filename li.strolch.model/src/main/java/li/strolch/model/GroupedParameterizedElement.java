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
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import li.strolch.exception.StrolchException;
import li.strolch.exception.StrolchModelException;
import li.strolch.model.parameter.Parameter;
import li.strolch.utils.helper.StringHelper;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public abstract class GroupedParameterizedElement extends AbstractStrolchElement implements ParameterBagContainer {

	private static final long serialVersionUID = 0L;

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
	 * @param name
	 * @param type
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
	 * Returns the {@link Parameter} with the given key from the {@link ParameterBag} with the given bagKey, or null if
	 * the {@link Parameter} or the {@link ParameterBag} does not exist
	 * 
	 * @param bagKey
	 *            the key of the {@link ParameterBag} from which the {@link Parameter} is to be returned
	 * @param paramKey
	 *            the key of the {@link Parameter} which is to be returned
	 * 
	 * @return the found {@link Parameter} or null if it was not found
	 */
	public <T extends Parameter<?>> T getParameter(String bagKey, String paramKey) {
		return getParameter(bagKey, paramKey, false);
	}

	/**
	 * Returns the {@link Parameter} with the given key from the {@link ParameterBag} with the given bagKey, or null if
	 * the {@link Parameter} or the {@link ParameterBag} does not exist
	 * 
	 * @param bagKey
	 *            the key of the {@link ParameterBag} from which the {@link Parameter} is to be returned
	 * @param paramKey
	 *            the key of the {@link Parameter} which is to be returned
	 * @param assertExists
	 *            if set to true, and the parameter does not exist, a {@link StrolchModelException} is thrown
	 * 
	 * @return the found {@link Parameter} or null if it was not found
	 */
	public <T extends Parameter<?>> T getParameter(String bagKey, String paramKey, boolean assertExists) {
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

	/**
	 * Adds a new {@link Parameter} to the {@link ParameterBag} with the given key
	 * 
	 * @param bagKey
	 *            the key of the {@link ParameterBag} to which the {@link Parameter} should be added
	 * @param parameter
	 *            the {@link Parameter} to be added to the {@link ParameterBag}
	 * 
	 * @throws StrolchException
	 *             if the {@link ParameterBag} does not exist
	 */
	public void addParameter(String bagKey, Parameter<?> parameter) throws StrolchException {
		if (this.parameterBagMap == null) {
			this.parameterBagMap = new HashMap<>();
		}
		ParameterBag bag = this.parameterBagMap.get(bagKey);
		if (bag == null) {
			String msg = "No parameter bag exists with key {0}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, bagKey);
			throw new StrolchException(msg);
		}

		bag.addParameter(parameter);
	}

	/**
	 * Removes the {@link Parameter} with the given paramKey from the {@link ParameterBag} with the given bagKey
	 * 
	 * @param bagKey
	 *            the key of the {@link ParameterBag} from which the {@link Parameter} is to be removed
	 * @param paramKey
	 *            the key of the {@link Parameter} which is to be removed
	 * 
	 * @return the removed {@link Parameter} or null if it did not exist
	 */
	public <T> Parameter<T> removeParameter(String bagKey, String paramKey) {
		if (this.parameterBagMap == null) {
			return null;
		}
		ParameterBag bag = this.parameterBagMap.get(bagKey);
		if (bag == null) {
			return null;
		}

		return bag.removeParameter(paramKey);
	}

	/**
	 * Returns the {@link ParameterBag} with the given key, or null if it does not exist
	 * 
	 * @param key
	 *            the key of the {@link ParameterBag} to return
	 * 
	 * @return the {@link ParameterBag} with the given key, or null if it does not exist
	 */
	public ParameterBag getParameterBag(String key) {
		if (this.parameterBagMap == null) {
			return null;
		}
		return this.parameterBagMap.get(key);
	}

	/**
	 * Adds the given {@link ParameterBag} to this {@link GroupedParameterizedElement}
	 * 
	 * @param bag
	 *            the {@link ParameterBag} to add
	 */
	public void addParameterBag(ParameterBag bag) {
		if (this.parameterBagMap == null) {
			this.parameterBagMap = new HashMap<>();
		}

		if (this.parameterBagMap.containsKey(bag.getId())) {
			String msg = "A ParameterBag already exists with id {0} on {1}";
			throw new StrolchException(MessageFormat.format(msg, bag.getId(), getLocator()));
		}
		this.parameterBagMap.put(bag.getId(), bag);
		bag.setParent(this);
	}

	/**
	 * Removes the {@link ParameterBag} with the given key
	 * 
	 * @param key
	 *            the key of the {@link ParameterBag} to remove
	 * 
	 * @return the removed {@link ParameterBag}, or null if it does not exist
	 */
	public ParameterBag removeParameterBag(String key) {
		if (this.parameterBagMap == null) {
			return null;
		}
		return this.parameterBagMap.remove(key);
	}

	/**
	 * Returns true if this {@link GroupedParameterizedElement} has any {@link ParameterBag ParameterBag}
	 * 
	 * @return true if this {@link GroupedParameterizedElement} has any {@link ParameterBag ParameterBag}
	 */
	public boolean hasParameterBags() {
		return this.parameterBagMap != null && !this.parameterBagMap.isEmpty();
	}

	/**
	 * Returns true if the {@link ParameterBag} with the given key exists on this {@link GroupedParameterizedElement}.
	 * 
	 * @param bagKey
	 *            the key of the {@link ParameterBag} which is to be checked for existence
	 * @return true if the {@link ParameterBag} with the given key exists on this {@link GroupedParameterizedElement}.
	 */
	public boolean hasParameterBag(String bagKey) {
		return this.parameterBagMap != null && this.parameterBagMap.containsKey(bagKey);
	}

	/**
	 * Returns true if the {@link Parameter} with the given paramKey exists on the {@link ParameterBag} with the given
	 * bagKey
	 * 
	 * @param bagKey
	 *            the key of the {@link ParameterBag} on which to find the {@link Parameter}
	 * @param paramKey
	 *            the key of the {@link Parameter} to be found
	 * 
	 * @return true if the {@link Parameter} with the given paramKey exists on the {@link ParameterBag} with the given
	 *         bagKey. False is returned if the {@link ParameterBag} does not exist, or the {@link Parameter} does not
	 *         exist on the {@link ParameterBag}
	 */
	public boolean hasParameter(String bagKey, String paramKey) {
		if (this.parameterBagMap == null) {
			return false;
		}
		ParameterBag bag = this.parameterBagMap.get(bagKey);
		if (bag == null) {
			return false;
		}

		return bag.hasParameter(paramKey);
	}

	/**
	 * Returns the {@link Set} of keys for the {@link ParameterBag}s on this {@link GroupedParameterizedElement}
	 * 
	 * @return the {@link Set} of keys for the {@link ParameterBag}s on this {@link GroupedParameterizedElement}
	 */
	public Set<String> getParameterBagKeySet() {
		if (this.parameterBagMap == null) {
			return Collections.emptySet();
		}
		return new HashSet<>(this.parameterBagMap.keySet());
	}

	/**
	 * Fills {@link GroupedParameterizedElement} properties of this clone
	 * 
	 * @param clone
	 */
	protected void fillClone(GroupedParameterizedElement clone) {
		super.fillClone(clone);
		clone.setType(getType());

		if (this.parameterBagMap != null) {
			for (ParameterBag bag : this.parameterBagMap.values()) {
				clone.addParameterBag(bag.getClone());
			}
		}
	}
}
