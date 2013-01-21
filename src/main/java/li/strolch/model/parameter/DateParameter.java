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

import java.text.DateFormat;

import org.w3c.dom.Element;

import li.strolch.exception.StrolchException;
import ch.eitchnet.utils.helper.StringHelper;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class DateParameter extends AbstractParameter<Long> {

	public static final String TYPE = "Date";
	private static final long serialVersionUID = 0L;

	private Long value;

	/**
	 * Empty constructor
	 */
	public DateParameter() {
		//
	}

	/**
	 * Default Constructor
	 * 
	 * @param id
	 * @param name
	 * @param value
	 */
	public DateParameter(String id, String name, Long value) {
		super(id, name);
		setValue(value);
	}

	/**
	 * DOM Constructor
	 * 
	 * @param element
	 */
	public DateParameter(Element element) {
		super.fromDom(element);

		String valueS = element.getAttribute("Value");
		if (StringHelper.isEmpty(valueS)) {
			throw new StrolchException("No value defined for " + this.id);
		}

		setValue(Long.valueOf(valueS));
	}

	@Override
	public String getValueAsString() {
		// TODO the format should be globally configured
		DateFormat dateFormat = DateFormat.getDateTimeInstance(DateFormat.SHORT, DateFormat.SHORT);
		return dateFormat.format(this.value);
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
		return DateParameter.TYPE;
	}

	@Override
	public Parameter<Long> getClone() {
		DateParameter clone = new DateParameter();

		super.fillClone(clone);

		clone.setValue(this.value);

		return clone;
	}
}
