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
public class StringParameter extends AbstractParameter<String> {

	public static final String UNDEFINED_VALUE = "-"; //$NON-NLS-1$
	public static final String TYPE = "String"; //$NON-NLS-1$
	private static final long serialVersionUID = 0L;

	private String value = UNDEFINED_VALUE;

	/**
	 * Empty constructor
	 * 
	 */
	public StringParameter() {
		//
	}

	/**
	 * Default constructor
	 * 
	 * @param id
	 * @param name
	 * @param value
	 */
	public StringParameter(String id, String name, String value) {
		super(id, name);
		setValue(value);
	}

	/**
	 * DOM Constructor
	 * 
	 * @param element
	 */
	public StringParameter(Element element) {
		super.fromDom(element);

		String valueS = element.getAttribute(Tags.VALUE);
		if (StringHelper.isEmpty(valueS)) {
			String msg = MessageFormat.format("No value defined for {0}", this.id); //$NON-NLS-1$
			throw new StrolchException(msg);
		}

		setValue(valueS);
	}

	@Override
	public String getType() {
		return StringParameter.TYPE;
	}

	@Override
	public String getValue() {
		return this.value;
	}

	@Override
	public String getValueAsString() {
		return this.value;
	}

	@Override
	public void setValue(String value) {
		validateValue(value);
		this.value = value;
	}

	@Override
	public Parameter<String> getClone() {
		StringParameter clone = new StringParameter();

		super.fillClone(clone);

		clone.setValue(this.value);

		return clone;
	}
}
