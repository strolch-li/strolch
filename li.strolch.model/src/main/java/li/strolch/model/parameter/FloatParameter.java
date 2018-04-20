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
import li.strolch.model.visitor.StrolchElementVisitor;
import li.strolch.utils.dbc.DBC;
import li.strolch.utils.helper.MathHelper;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
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
	 * 		the id
	 * @param name
	 * 		the name
	 * @param value
	 * 		the value
	 */
	public FloatParameter(String id, String name, Double value) {
		super(id, name);

		setValue(value);
	}

	@Override
	public String getValueAsString() {
		return Double.toString(this.value);
	}

	@SuppressWarnings("unchecked")
	@Override
	public Double getValue() {
		return this.value;
	}

	@Override
	public void setValue(Double value) {
		assertNotReadonly();
		validateValue(value);
		this.value = MathHelper.toPrecision(value, 8);
	}

	/**
	 * Sets the value to 0
	 *
	 * @see Parameter#clear()
	 */
	@Override
	public void clear() {
		assertNotReadonly();
		this.value = 0.0D;
	}

	@Override
	public boolean isEmpty() {
		return this.value == 0.0D;
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
	public <U> U accept(StrolchElementVisitor<U> visitor) {
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

	@Override
	public StrolchValueType getValueType() {
		return StrolchValueType.FLOAT;
	}
}
