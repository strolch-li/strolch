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
import java.util.Iterator;
import java.util.List;

import li.strolch.model.StrolchValueType;
import li.strolch.model.visitor.ParameterVisitor;
import li.strolch.utils.dbc.DBC;
import li.strolch.utils.helper.StringHelper;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class FloatListParameter extends AbstractParameter<List<Double>> implements ListParameter<Double> {

	private static final long serialVersionUID = 1L;

	protected List<Double> value;

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
	 * @param name
	 * @param value
	 */
	public FloatListParameter(String id, String name, List<Double> value) {
		super(id, name);

		setValue(value);
	}

	@Override
	public String getValueAsString() {
		if (this.value.isEmpty()) {
			return StringHelper.EMPTY;
		}

		StringBuilder sb = new StringBuilder();
		Iterator<Double> iter = this.value.iterator();
		while (iter.hasNext()) {

			sb.append(iter.next());

			if (iter.hasNext()) {
				sb.append(VALUE_SEPARATOR2);
				sb.append(" ");
			}
		}

		return sb.toString();
	}

	@Override
	public List<Double> getValue() {
		return new ArrayList<>(this.value);
	}

	@Override
	public void setValue(List<Double> value) {
		validateValue(value);
		if (this.value == null) {
			this.value = new ArrayList<>(value.size());
		}
		this.value.clear();
		this.value.addAll(value);
	}

	@Override
	public void setValueFromString(String valueAsString) {
		setValue(parseFromString(valueAsString));
	}

	@Override
	public void addValue(Double value) {
		this.value.add(value);
	}

	@Override
	public boolean removeValue(Double value) {
		return this.value.remove(value);
	}

	@Override
	public void clearValue() {
		this.value.clear();
	}

	@Override
	public boolean isValueEmpty() {
		return this.value.isEmpty();
	}

	@Override
	public String getType() {
		return StrolchValueType.FLOAT_LIST.getType();
	}

	@Override
	public FloatListParameter getClone() {
		FloatListParameter clone = new FloatListParameter();

		super.fillClone(clone);

		clone.setValue(this.value);

		return clone;
	}

	@Override
	public <U> U accept(ParameterVisitor visitor) {
		return visitor.visitFloatListParam(this);
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
		return Integer.valueOf(this.getValue().size()).compareTo(((FloatListParameter) o).getValue().size());
	}
}
