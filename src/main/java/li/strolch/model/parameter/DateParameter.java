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
import java.util.Date;

import li.strolch.exception.StrolchException;
import li.strolch.model.Tags;

import org.w3c.dom.Element;

import ch.eitchnet.utils.helper.StringHelper;
import ch.eitchnet.utils.iso8601.ISO8601FormatFactory;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class DateParameter extends AbstractParameter<Date> {

	public static final String TYPE = "Date"; //$NON-NLS-1$
	private static final long serialVersionUID = 0L;

	private Date value;

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
	public DateParameter(String id, String name, Date value) {
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

		String valueS = element.getAttribute(Tags.VALUE);
		if (StringHelper.isEmpty(valueS)) {
			String msg = MessageFormat.format("No value defined for {0}", this.id); //$NON-NLS-1$
			throw new StrolchException(msg);
		}

		setValue(parseFromString(valueS));
	}

	@Override
	public String getValueAsString() {
		return ISO8601FormatFactory.getInstance().formatDate(this.value);
	}

	@Override
	public Date getValue() {
		return this.value;
	}

	@Override
	public void setValue(Date value) {
		validateValue(value);
		this.value = value;
	}

	@Override
	public String getType() {
		return DateParameter.TYPE;
	}

	@Override
	public Parameter<Date> getClone() {
		DateParameter clone = new DateParameter();

		super.fillClone(clone);

		clone.setValue(this.value);

		return clone;
	}

	public static Date parseFromString(String valueS) {
		return ISO8601FormatFactory.getInstance().getDateFormat().parse(valueS);
	}
}
