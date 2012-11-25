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

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import li.strolch.model.StrolchElement;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class StringListParameter extends AbstractParameter<List<String>> implements ListParameter<String> {

	public static final String TYPE = "StringList";
	private static final long serialVersionUID = 1L;

	protected List<String> value;

	@Override
	public String getValueAsString() {
		if (this.value.isEmpty())
			return "";

		StringBuilder sb = new StringBuilder();
		Iterator<String> iter = this.value.iterator();
		while (iter.hasNext()) {

			sb.append(iter.next());

			if (iter.hasNext())
				sb.append(";");
		}

		return sb.toString();
	}

	@Override
	public List<String> getValue() {
		return new ArrayList<String>(this.value);
	}

	@Override
	public void setValue(List<String> value) {
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
