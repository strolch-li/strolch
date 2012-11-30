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

import org.dom4j.Element;

import ch.eitchnet.utils.helper.StringHelper;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public class LongParameter extends AbstractParameter<Long> {

	public static final String TYPE = "Long";
	private static final long serialVersionUID = 0L;

	protected Long value;

	/**
	 * Empty constructor
	 */
	public LongParameter() {
		//
	}

	/**
	 * Default constructor
	 * 
	 * @param id
	 * @param name
	 * @param value
	 */
	public LongParameter(String id, String name, Long value) {
		super(id, name);
		setValue(Long.valueOf(value));
	}

	/**
	 * DOM Constructor
	 * 
	 * @param element
	 */
	public LongParameter(Element element) {
		super.fromDom(element);

		String valueS = element.attributeValue("Value");
		if (StringHelper.isEmpty(valueS)) {
			throw new StrolchException("No value defined for " + this.id);
		}

		setValue(Long.valueOf(valueS));
	}

	@Override
	public String getValueAsString() {
		return this.value.toString();
	}

	@Override
	public Long getValue() {
		return this.value;
	}

	@Override
	public void setValue(Long value) {
		validateValue(value);
		this.value = value;
	}

	@Override
	public String getType() {
		return LongParameter.TYPE;
	}

	@Override
	public Parameter<Long> getClone() {
		LongParameter clone = new LongParameter();

		super.fillClone(clone);

		clone.setValue(this.value);

		return clone;
	}
}
