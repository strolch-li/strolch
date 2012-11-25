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
package li.strolch.model.parameter;

import li.strolch.exception.StrolchException;
import li.strolch.model.Parameter;

import org.dom4j.Element;

import ch.eitchnet.utils.helper.StringHelper;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class BooleanParameter extends AbstractParameter<Boolean> {

	public static final String TYPE = "Boolean";
	private static final long serialVersionUID = 0L;

	private Boolean value = Boolean.FALSE;

	/**
	 * Empty constructor
	 */
	public BooleanParameter() {
		//
	}

	/**
	 * Default constructors
	 * 
	 * @param id
	 * @param name
	 * @param value
	 */
	public BooleanParameter(String id, String name, Boolean value) {
		super(id, name);
		setValue(value);
	}

	/**
	 * DOM Constructor
	 * 
	 * @param element
	 */
	public BooleanParameter(Element element) {
		super.fromDom(element);

		String valueS = element.attributeValue("Value");
		if (StringHelper.isEmpty(valueS)) {
			throw new StrolchException("No value defined for " + this.id);
		}

		setValue(Boolean.valueOf(valueS));
	}

	@Override
	public String getValueAsString() {
		return this.value.toString();
	}

	@Override
	public Boolean getValue() {
		return this.value;
	}

	@Override
	public void setValue(Boolean value) {
		validateValue(value);
		this.value = value;
	}

	@Override
	public String getType() {
		return BooleanParameter.TYPE;
	}

	@Override
	public Parameter<Boolean> getClone() {
		BooleanParameter clone = new BooleanParameter();

		super.fillClone(clone);

		clone.setValue(this.value);

		return clone;
	}
}
