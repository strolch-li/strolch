/*
 * Copyright (c) 2012, Robert von Burg
 *
 * All rights reserved.
 *
 * This file is part of the li.strolch.model.
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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import org.w3c.dom.Element;

import li.strolch.exception.StrolchException;
import li.strolch.model.StrolchElement;
import li.strolch.model.Tags;
import ch.eitchnet.utils.helper.StringHelper;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class StringListParameter extends AbstractParameter<List<String>> implements ListParameter<String> {

	public static final String TYPE = "StringList"; //$NON-NLS-1$
	private static final long serialVersionUID = 1L;

	protected List<String> value;

	/**
	 * Empty constructor
	 */
	public StringListParameter() {
		//
	}

	/**
	 * Default constructor
	 * 
	 * @param id
	 * @param name
	 * @param value
	 */
	public StringListParameter(String id, String name, List<String> value) {
		super(id, name);

		setValue(value);
	}

	/**
	 * DOM Constructor
	 * 
	 * @param element
	 */
	public StringListParameter(Element element) {
		super.fromDom(element);

		String valueS = element.getAttribute(Tags.VALUE);
		if (StringHelper.isEmpty(valueS)) {
			String msg = MessageFormat.format("No value defined for {0}", this.id); //$NON-NLS-1$
			throw new StrolchException(msg);
		}

		setValue(parse(valueS));
	}

	private List<String> parse(String value) {
		if (value.isEmpty())
			return Collections.emptyList();

		String[] valueArr = value.split(VALUE_SEPARATOR);
		return Arrays.asList(valueArr);
	}

	@Override
	public String getValueAsString() {
		if (this.value.isEmpty())
			return StringHelper.EMPTY;

		StringBuilder sb = new StringBuilder();
		Iterator<String> iter = this.value.iterator();
		while (iter.hasNext()) {

			sb.append(iter.next());

			if (iter.hasNext())
				sb.append(VALUE_SEPARATOR);
		}

		return sb.toString();
	}

	@Override
	public List<String> getValue() {
		return new ArrayList<String>(this.value);
	}

	@Override
	public void setValue(List<String> value) {
		validateValue(value);
		if (this.value == null)
			this.value = new ArrayList<String>(value.size());
		this.value.clear();
		this.value.addAll(value);
	}

	@Override
	public void addValue(String value) {
		this.value.add(value);
	}

	@Override
	public boolean removeValue(String value) {
		return this.value.remove(value);
	}

	@Override
	public String getType() {
		return TYPE;
	}

	@Override
	public StrolchElement getClone() {
		StringListParameter clone = new StringListParameter();

		super.fillClone(clone);

		clone.setValue(this.value);

		return clone;
	}
}
