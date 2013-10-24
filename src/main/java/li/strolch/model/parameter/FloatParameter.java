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

import java.text.MessageFormat;

import li.strolch.exception.StrolchException;
import li.strolch.model.Tags;

import org.w3c.dom.Element;

import ch.eitchnet.utils.helper.StringHelper;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public class FloatParameter extends AbstractParameter<Double> {

	public static final String TYPE = "Float"; //$NON-NLS-1$
	private static final long serialVersionUID = 0L;

	private Double value = Double.MAX_VALUE;

	/**
	 * Empty constructor
	 */
	public FloatParameter() {
		//
	}

	/**
	 * Default constructor
	 * 
	 * @param id
	 * @param name
	 * @param value
	 */
	public FloatParameter(String id, String name, Double value) {
		super(id, name);

		setValue(value);
	}

	/**
	 * DOM Constructor
	 * 
	 * @param element
	 */
	public FloatParameter(Element element) {
		super.fromDom(element);

		String valueS = element.getAttribute(Tags.VALUE);
		if (StringHelper.isEmpty(valueS)) {
			String msg = MessageFormat.format("No value defined for {0}", this.id); //$NON-NLS-1$
			throw new StrolchException(msg);
		}

		setValue(Double.valueOf(valueS));
	}

	@Override
	public String getValueAsString() {
		return Double.toString(this.value);
	}

	@Override
	public Double getValue() {
		return this.value;
	}

	@Override
	public void setValue(Double value) {
		validateValue(value);
		this.value = value;
	}

	@Override
	public String getType() {
		return FloatParameter.TYPE;
	}

	@Override
	public Parameter<Double> getClone() {
		FloatParameter clone = new FloatParameter();

		super.fillClone(clone);

		clone.setValue(this.value);

		return clone;
	}
}
