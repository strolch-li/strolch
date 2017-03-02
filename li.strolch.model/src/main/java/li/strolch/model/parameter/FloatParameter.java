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

import li.strolch.model.StrolchValueType;
import li.strolch.model.visitor.ParameterVisitor;
import li.strolch.utils.dbc.DBC;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 *
 */
public class FloatParameter extends AbstractParameter<Double> {

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

	/**
	 * Sets the value to 0
	 * 
	 * @see Parameter#clearValue()
	 */
	@Override
	public void clearValue() {
		this.value = 0.0D;
	}

	@Override
	public void setValueFromString(String valueAsString) {
		setValue(parseFromString(valueAsString));
	}

	@Override
	public String getType() {
		return StrolchValueType.FLOAT.getType();
	}

	@Override
	public FloatParameter getClone() {
		FloatParameter clone = new FloatParameter();

		super.fillClone(clone);

		clone.setValue(this.value);

		return clone;
	}

	@Override
	public <U> U accept(ParameterVisitor visitor) {
		return visitor.visitFloatParam(this);
	}

	public static Double parseFromString(String valueS) {
		return Double.valueOf(valueS);
	}

	@Override
	public int compareTo(Parameter<?> o) {
		DBC.PRE.assertEquals("Not same Parameter types!", this.getType(), o.getType());
		return this.getValue().compareTo(((FloatParameter) o).getValue());
	}
}
