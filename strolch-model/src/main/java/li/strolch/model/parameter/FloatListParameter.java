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
public class FloatListParameter extends AbstractListParameter<Double> {

	/**
	 * Empty constructor
	 */
	public FloatListParameter() {
		//
	}

	/**
	 * Default constructor
	 *
	 * @param id
	 * 		the id
	 * @param name
	 * 		the name
	 * @param values
	 * 		the values
	 */
	public FloatListParameter(String id, String name, List<Double> values) {
		super(id, name);

		setValue(values);
	}

	@Override
	protected String elementToString(Double element) {
		return element.toString();
	}

	@Override
	protected void validateElement(Double value) {
		DBC.PRE.assertNotNull("null values not allowed!", value);
	}

	@Override
	public String getType() {
		return StrolchValueType.FLOAT_LIST.getType();
	}

	@Override
	public StrolchValueType getValueType() {
		return StrolchValueType.FLOAT_LIST;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null || getClass() != obj.getClass())
			return false;
		FloatListParameter o = (FloatListParameter) obj;
		return this.parent == o.parent && this.id.equals(o.id);
	}

	@Override
	public int hashCode() {
		return Objects.hash(parent, id);
	}

	@Override
	public FloatListParameter getClone() {
		FloatListParameter clone = new FloatListParameter();
		super.fillClone(clone);
		clone.value = new ArrayList<>(this.value);
		return clone;
	}

	@Override
	public <U> U accept(StrolchElementVisitor<U> visitor) {
		return visitor.visitFloatListParam(this);
	}

	@Override
	protected List<Double> parseString(String value) {
		return parseFromString(value);
	}

	public static List<Double> parseFromString(String value) {
		if (value.isEmpty()) {
			return Collections.emptyList();
		}

		String[] valueArr;
		if (value.contains(VALUE_SEPARATOR1))
			valueArr = value.split(VALUE_SEPARATOR1);
		else
			valueArr = value.split(VALUE_SEPARATOR2);

		List<Double> values = new ArrayList<>();
		for (String val : valueArr) {
			values.add(Double.valueOf(val.trim()));
		}
		return values;
	}

	@Override
	public int compareTo(Parameter<?> o) {
		DBC.PRE.assertEquals("Not same Parameter types!", this.getType(), o.getType());
		return Integer.compare(this.getValue().size(), ((FloatListParameter) o).getValue().size());
	}
}
