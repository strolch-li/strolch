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

import java.text.MessageFormat;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import li.strolch.exception.StrolchException;
import li.strolch.model.parameter.Parameter;

import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import ch.eitchnet.utils.helper.StringHelper;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public abstract class GroupedParameterizedElement extends AbstractStrolchElement {

	private static final long serialVersionUID = 0L;

	protected Map<String, ParameterBag> parameterBagMap;
	protected String type;

	/**
	 * Empty constructor
	 */
	protected GroupedParameterizedElement() {
		//
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
	public <T> Parameter<T> getParameter(String bagKey, String paramKey) {
		if (this.parameterBagMap == null)
			return null;
		ParameterBag bag = this.parameterBagMap.get(bagKey);
		if (bag == null)
			return null;

		return bag.getParameter(paramKey);
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
		if (this.parameterBagMap == null)
			this.parameterBagMap = new HashMap<String, ParameterBag>();
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
		if (this.parameterBagMap == null)
			return null;
		ParameterBag bag = this.parameterBagMap.get(bagKey);
		if (bag == null)
			return null;

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
		if (this.parameterBagMap == null)
			return null;
		return this.parameterBagMap.get(key);
	}

	/**
	 * Adds the given {@link ParameterBag} to this {@link GroupedParameterizedElement}
	 * 
	 * @param bag
	 *            the {@link ParameterBag} to add
	 */
	public void addParameterBag(ParameterBag bag) {
		if (this.parameterBagMap == null)
			this.parameterBagMap = new HashMap<String, ParameterBag>();
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
		if (this.parameterBagMap == null)
			return null;
		return this.parameterBagMap.remove(key);
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
		if (this.parameterBagMap == null)
			return false;
		ParameterBag bag = this.parameterBagMap.get(bagKey);
		if (bag == null)
			return false;

		return bag.hasParameter(paramKey);
	}

	/**
	 * Returns the {@link Set} of keys for the {@link ParameterBag}s on this {@link GroupedParameterizedElement}
	 * 
	 * @return the {@link Set} of keys for the {@link ParameterBag}s on this {@link GroupedParameterizedElement}
	 */
	public Set<String> getParameterBagKeySet() {
		if (this.parameterBagMap == null)
			return Collections.emptySet();
		return new HashSet<String>(this.parameterBagMap.keySet());
	}

	@Override
	public void fromDom(Element element) {
		super.fromDom(element);

		String type = element.getAttribute(Tags.TYPE);
		setType(type);

		NodeList bags = element.getElementsByTagName(Tags.PARAMETER_BAG);
		for (int i = 0; i < bags.getLength(); i++) {
			Element bagElement = (Element) bags.item(i);
			ParameterBag bag = new ParameterBag(bagElement);
			addParameterBag(bag);
		}
	}

	@Override
	protected void fillElement(Element element) {
		super.fillElement(element);

		if (this.parameterBagMap != null) {
			for (ParameterBag bag : this.parameterBagMap.values()) {
				element.appendChild(bag.toDom(element.getOwnerDocument()));
			}
		}
	}

	/**
	 * Fills {@link GroupedParameterizedElement} properties of this clone
	 * 
	 * @param clone
	 */
	protected void fillClone(GroupedParameterizedElement clone) {
		super.fillClone(clone);

		if (this.parameterBagMap != null) {
			for (ParameterBag bag : this.parameterBagMap.values()) {
				clone.addParameterBag(bag.getClone());
			}
		}
	}
}
