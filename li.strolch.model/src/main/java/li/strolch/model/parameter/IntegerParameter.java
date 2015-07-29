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

import ch.eitchnet.utils.dbc.DBC;
import li.strolch.model.StrolchValueType;
import li.strolch.model.visitor.ParameterVisitor;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 *
 */
public class IntegerParameter extends AbstractParameter<Integer> {

	private static final long serialVersionUID = 0L;

	private Integer value = Integer.MAX_VALUE;

	/**
	 * Empty constructor
	 */
	public IntegerParameter() {
		//
	}

	/**
	 * Default constructor
	 *
	 * @param id
	 * @param name
	 * @param value
	 */
	public IntegerParameter(String id, String name, Integer value) {
		super(id, name);
		setValue(value);
	}

	@Override
	public String getType() {
		return StrolchValueType.INTEGER.getType();
	}

	@Override
	public String getValueAsString() {
		return Integer.toString(this.value);
	}

	@Override
	public Integer getValue() {
		return this.value;
	}

	@Override
	public void setValue(Integer value) {
		validateValue(value);
		this.value = value;
	}

	@Override
	public void setValueFromString(String valueAsString) {
		setValue(parseFromString(valueAsString));
	}

	@Override
	public IntegerParameter getClone() {
		IntegerParameter clone = new IntegerParameter();

		super.fillClone(clone);

		clone.setValue(this.value);

		return clone;
	}

	@Override
	public <U> U accept(ParameterVisitor visitor) {
		return visitor.visitIntegerParam(this);
	}

	public static Integer parseFromString(String valueS) {
		return Integer.valueOf(valueS);
	}

	@Override
	public int compareTo(Parameter<?> o) {
		DBC.PRE.assertEquals("Not same Parameter types!", this.getType(), o.getType());
		return this.getValue().compareTo(((IntegerParameter) o).getValue());
	}
}
