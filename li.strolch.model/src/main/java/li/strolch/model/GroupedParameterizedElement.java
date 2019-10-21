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
import static li.strolch.model.StrolchModelConstants.BAG_PARAMETERS;
import static li.strolch.model.StrolchModelConstants.BAG_RELATIONS;

import java.text.MessageFormat;
import java.util.*;
import java.util.stream.Stream;

import li.strolch.exception.StrolchException;
import li.strolch.exception.StrolchModelException;
import li.strolch.model.parameter.Parameter;
import li.strolch.model.parameter.StringListParameter;
import li.strolch.model.parameter.StringParameter;
import li.strolch.utils.helper.StringHelper;

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
	 * 		the key of the {@link ParameterBag} from which the {@link Parameter} is to be returned
	 * @param paramKey
	 * 		the key of the {@link Parameter} which is to be returned
	 *
	 * @return the found {@link Parameter} or null if it was not found
	 */
	@Override
	public <U, T extends Parameter<U>> T getParameter(String bagKey, String paramKey) {
		return getParameter(bagKey, paramKey, false);
	}

	/**
	 * Returns the {@link Parameter} with the given key from the {@link ParameterBag} with the given bagKey, or null if
	 * the {@link Parameter} or the {@link ParameterBag} does not exist
	 *
	 * @param bagKey
	 * 		the key of the {@link ParameterBag} from which the {@link Parameter} is to be returned
	 * @param paramKey
	 * 		the key of the {@link Parameter} which is to be returned
	 * @param assertExists
	 * 		if set to true, and the parameter does not exist, a {@link StrolchModelException} is thrown
	 *
	 * @return the found {@link Parameter} or null if it was not found
	 *
	 * @throws StrolchModelException
	 * 		if the element does not exist and {@code assertExists} is true
	 */
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

	/**
	 * Returns the {@link Parameter} with the given key from the {@link ParameterBag} with the ID {@link
	 * StrolchModelConstants#BAG_PARAMETERS}, or null if the {@link Parameter} or the {@link ParameterBag} does not
	 * exist
	 *
	 * @param paramKey
	 * 		the key of the {@link Parameter} which is to be returned
	 *
	 * @return the found {@link Parameter} or null if it was not found
	 */
	@Override
	public <U, T extends Parameter<U>> T getParameter(String paramKey) {
		return getParameter(BAG_PARAMETERS, paramKey, false);
	}

	/**
	 * Returns the {@link Parameter} with the given key from the {@link ParameterBag} with the ID {@link
	 * StrolchModelConstants#BAG_PARAMETERS}, or null if the {@link Parameter} or the {@link ParameterBag} does not
	 * exist
	 *
	 * @param paramKey
	 * 		the key of the {@link Parameter} which is to be returned
	 * @param assertExists
	 * 		if set to true, and the parameter does not exist, a {@link StrolchModelException} is thrown
	 *
	 * @return the found {@link Parameter} or null if it was not found
	 */
	@Override
	public <U, T extends Parameter<U>> T getParameter(String paramKey, boolean assertExists) {
		return getParameter(BAG_PARAMETERS, paramKey, assertExists);
	}

	/**
	 * Returns the {@link StringParameter} with the given key from the {@link ParameterBag} with the ID {@link
	 * StrolchModelConstants#BAG_RELATIONS}, or null if the {@link Parameter} or the {@link ParameterBag} does not
	 * exist
	 *
	 * @param paramKey
	 * 		the key of the {@link Parameter} which is to be returned
	 *
	 * @return the found {@link Parameter} or null if it was not found
	 */
	@Override
	public StringParameter getRelationParam(String paramKey) {
		return getParameter(BAG_RELATIONS, paramKey, false);
	}

	/**
	 * Returns the {@link StringParameter} with the given key from the {@link ParameterBag} with the ID {@link
	 * StrolchModelConstants#BAG_RELATIONS}, or null if the {@link Parameter} or the {@link ParameterBag} does not
	 * exist
	 *
	 * @param paramKey
	 * 		the key of the {@link Parameter} which is to be returned
	 * @param assertExists
	 * 		if set to true, and the parameter does not exist, a {@link StrolchModelException} is thrown
	 *
	 * @return the found {@link Parameter} or null if it was not found
	 */
	@Override
	public StringParameter getRelationParam(String paramKey, boolean assertExists) {
		return getParameter(BAG_RELATIONS, paramKey, assertExists);
	}

	/**
	 * Returns the {@link StringListParameter} with the given key from the {@link ParameterBag} with the ID {@link
	 * StrolchModelConstants#BAG_RELATIONS}, or null if the {@link Parameter} or the {@link ParameterBag} does not
	 * exist
	 *
	 * @param paramKey
	 * 		the key of the {@link Parameter} which is to be returned
	 *
	 * @return the found {@link Parameter} or null if it was not found
	 */
	@Override
	public StringListParameter getRelationsParam(String paramKey) {
		return getParameter(BAG_RELATIONS, paramKey, false);
	}

	/**
	 * Returns the {@link StringListParameter} with the given key from the {@link ParameterBag} with the ID {@link
	 * StrolchModelConstants#BAG_RELATIONS}, or null if the {@link Parameter} or the {@link ParameterBag} does not
	 * exist
	 *
	 * @param paramKey
	 * 		the key of the {@link Parameter} which is to be returned
	 * @param assertExists
	 * 		if set to true, and the parameter does not exist, a {@link StrolchModelException} is thrown
	 *
	 * @return the found {@link Parameter} or null if it was not found
	 */
	@Override
	public StringListParameter getRelationsParam(String paramKey, boolean assertExists) {
		return getParameter(BAG_RELATIONS, paramKey, assertExists);
	}

	/**
	 * Returns a {@link Stream} of all the {@link Parameter Parameters}
	 *
	 * @param bagKey
	 * 		the key of the {@link ParameterBag} from which the {@link Parameter} is to be returned
	 *
	 * @return the parameters with the given interpretation
	 */
	@Override
	public Stream<Parameter<?>> streamOfParameters(String bagKey) {
		ParameterBag bag = getParameterBag(bagKey);
		if (bag == null)
			return Stream.empty();

		return bag.streamOfParameters();
	}

	/**
	 * Returns a {@link Stream} of all the {@link Parameter Parameters} with the given interpretation
	 *
	 * @param bagKey
	 * 		the key of the {@link ParameterBag} from which the {@link Parameter} is to be returned
	 * @param interpretation
	 * 		the interpretation for which the parameters are to be returned
	 *
	 * @return the parameters with the given interpretation
	 */
	@Override
	public Stream<Parameter<?>> streamOfParametersByInterpretation(String bagKey, String interpretation) {
		ParameterBag bag = getParameterBag(bagKey);
		if (bag == null)
			return Stream.empty();

		return bag.streamOfParametersByInterpretation(interpretation);
	}

	/**
	 * Returns a {@link Stream} of all the {@link Parameter Parameters} with the given interpretation
	 *
	 * @param bagKey
	 * 		the key of the {@link ParameterBag} from which the {@link Parameter} is to be returned
	 * @param interpretation
	 * 		the interpretation for which the parameters are to be returned
	 * @param uom
	 * 		the uom for which the parameters are to be returned
	 *
	 * @return the parameters with the given interpretation
	 */
	@Override
	public Stream<Parameter<?>> streamOfParametersByInterpretationAndUom(String bagKey, String interpretation,
			String uom) {
		ParameterBag bag = getParameterBag(bagKey);
		if (bag == null)
			return Stream.empty();

		return bag.streamOfParametersByInterpretationAndUom(interpretation, uom);
	}

	/**
	 * Returns a list of all the {@link Parameter Parameters} with the given interpretation
	 *
	 * @param bagKey
	 * 		the key of the {@link ParameterBag} from which the {@link Parameter} is to be returned
	 * @param interpretation
	 * 		the interpretation for which the parameters are to be returned
	 *
	 * @return the parameters with the given interpretation
	 */
	@Override
	public List<Parameter<?>> getParametersByInterpretation(String bagKey, String interpretation) {
		return streamOfParametersByInterpretation(bagKey, interpretation).collect(toList());
	}

	/**
	 * Returns a list of all the {@link Parameter Parameters} with the given interpretation
	 *
	 * @param bagKey
	 * 		the key of the {@link ParameterBag} from which the {@link Parameter} is to be returned
	 * @param interpretation
	 * 		the interpretation for which the parameters are to be returned
	 * @param uom
	 * 		the uom for which the parameters are to be returned
	 *
	 * @return the parameters with the given interpretation
	 */
	@Override
	public List<Parameter<?>> getParametersByInterpretationAndUom(String bagKey, String interpretation, String uom) {
		return streamOfParametersByInterpretationAndUom(bagKey, interpretation, uom).collect(toList());
	}

	/**
	 * Adds a new {@link Parameter} to the {@link ParameterBag} with the given key
	 *
	 * @param bagKey
	 * 		the key of the {@link ParameterBag} to which the {@link Parameter} should be added
	 * @param parameter
	 * 		the {@link Parameter} to be added to the {@link ParameterBag}
	 *
	 * @throws StrolchException
	 * 		if the {@link ParameterBag} does not exist
	 */
	@Override
	public void addParameter(String bagKey, Parameter<?> parameter) throws StrolchException {
		assertNotReadonly();
		if (this.parameterBagMap == null)
			this.parameterBagMap = new HashMap<>(1, 1.0F);
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
	 * 		the key of the {@link ParameterBag} from which the {@link Parameter} is to be removed
	 * @param paramKey
	 * 		the key of the {@link Parameter} which is to be removed
	 *
	 * @return the removed {@link Parameter} or null if it did not exist
	 */
	@Override
	public <U, T extends Parameter<U>> T removeParameter(String bagKey, String paramKey) {
		assertNotReadonly();
		if (this.parameterBagMap == null)
			return null;
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
	 * 		the key of the {@link ParameterBag} to return
	 *
	 * @return the {@link ParameterBag} with the given key, or null if it does not exist
	 */
	@Override
	public ParameterBag getParameterBag(String key) {
		return getParameterBag(key, false);
	}

	/**
	 * Returns the {@link ParameterBag} with the given key, or null if it does not exist
	 *
	 * @param key
	 * 		the key of the {@link ParameterBag} to return
	 * @param assertExists
	 * 		if set to true, and the parameter bag does not exist, a {@link StrolchModelException} is thrown
	 *
	 * @return the {@link ParameterBag} with the given key, or null if it does not exist
	 */
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

	/**
	 * Returns a {@link Stream} of {@link ParameterBag ParameterBags}
	 *
	 * @return the {@link ParameterBag ParameterBags}
	 */
	@Override
	public Stream<ParameterBag> streamOfParameterBags() {
		if (this.parameterBagMap == null || this.parameterBagMap.isEmpty())
			return Stream.empty();
		return this.parameterBagMap.values().stream();
	}

	/**
	 * Returns a {@link Stream} of {@link ParameterBag ParameterBags} of the given type
	 *
	 * @param type
	 * 		the type of {@link ParameterBag} to return
	 *
	 * @return the {@link ParameterBag ParameterBags} of the given type
	 */
	@Override
	public Stream<ParameterBag> streamOfParameterBagsByType(String type) {
		if (this.parameterBagMap == null || this.parameterBagMap.isEmpty())
			return Stream.empty();
		return this.parameterBagMap.values() //
				.stream() //
				.filter(map -> map.getType().equals(type));
	}

	/**
	 * Returns the {@link ParameterBag ParameterBags} of the given type
	 *
	 * @param type
	 * 		the type of {@link ParameterBag} to return
	 *
	 * @return the {@link ParameterBag ParameterBags} of the given type
	 */
	@Override
	public List<ParameterBag> getParameterBagsByType(String type) {
		return streamOfParameterBagsByType(type).collect(toList());
	}

	/**
	 * Adds the given {@link ParameterBag} to this {@link GroupedParameterizedElement}
	 *
	 * @param bag
	 * 		the {@link ParameterBag} to add
	 */
	@Override
	public void addParameterBag(ParameterBag bag) {
		assertNotReadonly();
		if (this.parameterBagMap == null) {
			this.parameterBagMap = new HashMap<>(1, 1.0F);
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
	 * 		the key of the {@link ParameterBag} to remove
	 *
	 * @return the removed {@link ParameterBag}, or null if it does not exist
	 */
	@Override
	public ParameterBag removeParameterBag(String key) {
		assertNotReadonly();
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
	@Override
	public boolean hasParameterBags() {
		return this.parameterBagMap != null && !this.parameterBagMap.isEmpty();
	}

	/**
	 * Returns true if the {@link ParameterBag} with the given key exists on this {@link GroupedParameterizedElement}.
	 *
	 * @param bagKey
	 * 		the key of the {@link ParameterBag} which is to be checked for existence
	 *
	 * @return true if the {@link ParameterBag} with the given key exists on this {@link GroupedParameterizedElement}.
	 */
	@Override
	public boolean hasParameterBag(String bagKey) {
		return this.parameterBagMap != null && this.parameterBagMap.containsKey(bagKey);
	}

	/**
	 * Returns true if the {@link Parameter} with the given paramKey exists on the {@link ParameterBag} with the given
	 * bagKey
	 *
	 * @param bagKey
	 * 		the key of the {@link ParameterBag} on which to find the {@link Parameter}
	 * @param paramKey
	 * 		the key of the {@link Parameter} to be found
	 *
	 * @return true if the {@link Parameter} with the given paramKey exists on the {@link ParameterBag} with the given
	 * bagKey. False is returned if the {@link ParameterBag} does not exist, or the {@link Parameter} does not exist on
	 * the {@link ParameterBag}
	 */
	@Override
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
	@Override
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
	 * 		the clone to fill
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

	@Override
	public void setReadOnly() {
		if (this.parameterBagMap != null) {
			for (ParameterBag bag : this.parameterBagMap.values()) {
				bag.setReadOnly();
			}
		}
		super.setReadOnly();
	}
}
