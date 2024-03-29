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
package li.strolch.model.parameter;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

import li.strolch.model.StrolchValueType;
import li.strolch.model.visitor.StrolchElementVisitor;
import li.strolch.utils.dbc.DBC;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class LongListParameter extends AbstractListParameter<Long> {

	/**
	 * Empty constructor
	 */
	public LongListParameter() {
		//
	}

	/**
	 * Default constructor
	 *
	 * @param id
	 * 		the id
	 * @param name
	 * 		the name
	 * @param value
	 * 		the value
	 */
	public LongListParameter(String id, String name, List<Long> value) {
		super(id, name);

		setValue(value);
	}

	@Override
	protected String elementToString(Long element) {
		return element.toString();
	}

	@Override
	protected void validateElement(Long value) {
		DBC.PRE.assertNotNull("null values not allowed!", value);
	}

	@Override
	public String getType() {
		return StrolchValueType.LONG_LIST.getType();
	}

	@Override
	public StrolchValueType getValueType() {
		return StrolchValueType.LONG_LIST;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null || getClass() != obj.getClass())
			return false;
		LongListParameter o = (LongListParameter) obj;
		return this.parent == o.parent && this.id.equals(o.id);
	}

	@Override
	public int hashCode() {
		return Objects.hash(parent, id);
	}

	@Override
	public LongListParameter getClone() {
		LongListParameter clone = new LongListParameter();
		super.fillClone(clone);
		clone.value = new ArrayList<>(this.value);
		return clone;
	}

	@Override
	public <U> U accept(StrolchElementVisitor<U> visitor) {
		return visitor.visitLongListParam(this);
	}

	@Override
	protected List<Long> parseString(String value) {
		return parseFromString(value);
	}

	public static List<Long> parseFromString(String value) {
		if (value.isEmpty()) {
			return Collections.emptyList();
		}

		String[] valueArr;
		if (value.contains(VALUE_SEPARATOR1))
			valueArr = value.split(VALUE_SEPARATOR1);
		else
			valueArr = value.split(VALUE_SEPARATOR2);

		List<Long> values = new ArrayList<>();
		for (String val : valueArr) {
			values.add(Long.valueOf(val.trim()));
		}
		return values;
	}

	@Override
	public int compareTo(Parameter<?> o) {
		DBC.PRE.assertEquals("Not same Parameter types!", this.getType(), o.getType());
		return Integer.compare(this.getValue().size(), ((LongListParameter) o).getValue().size());
	}
}
