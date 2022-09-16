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
import li.strolch.utils.helper.StringHelper;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class BooleanParameter extends AbstractParameter<Boolean> {

	private Boolean value = Boolean.FALSE;

	/**
	 * Empty constructor
	 */
	public BooleanParameter() {
		//
	}

	/**
	 * Default constructors
	 *
	 * @param id
	 * 		the id
	 * @param name
	 * 		the name
	 * @param value
	 * 		the value
	 */
	public BooleanParameter(String id, String name, Boolean value) {
		super(id, name);
		setValue(value);
	}

	@Override
	public String getValueAsString() {
		return this.value.toString();
	}

	@SuppressWarnings("unchecked")
	@Override
	public Boolean getValue() {
		return this.value;
	}

	@Override
	public void setValue(Boolean value) {
		assertNotReadonly();
		validateValue(value);
		this.value = value;
	}

	@Override
	public void setValueFrom(Parameter<Boolean> parameter) {
		assertNotReadonly();
		this.value = parameter.getValue();
	}

	@Override
	public void setValueFromString(String valueAsString) {
		setValue(parseFromString(valueAsString));
	}

	/**
	 * Sets the value to false
	 *
	 * @see Parameter#clear()
	 */
	@Override
	public void clear() {
		assertNotReadonly();
		this.value = false;
	}

	/**
	 * @return true if the value is false
	 */
	@Override
	public boolean isEmpty() {
		return !this.value;
	}

	@Override
	public boolean isEqualTo(Parameter<Boolean> otherValue) {
		BooleanParameter other = (BooleanParameter) otherValue;
		return this.value.equals(other.value);
	}

	@Override
	public boolean isEqualTo(Boolean otherValue) {
		return this.value.equals(otherValue);
	}

	public void flip() {
		assertNotReadonly();
		this.value = !this.value;
	}

	@Override
	public String getType() {
		return StrolchValueType.BOOLEAN.getType();
	}

	@Override
	public StrolchValueType getValueType() {
		return StrolchValueType.BOOLEAN;
	}

	@Override
	public BooleanParameter getClone() {
		BooleanParameter clone = new BooleanParameter();
		super.fillClone(clone);
		clone.value = this.value;
		return clone;
	}

	@Override
	public <U> U accept(StrolchElementVisitor<U> visitor) {
		return visitor.visitBooleanParam(this);
	}

	public static Boolean parseFromString(String valueS) {
		return StringHelper.parseBoolean(valueS);
	}

	@Override
	public int compareTo(Parameter<?> o) {
		DBC.PRE.assertEquals("Not same Parameter types!", this.getType(), o.getType());
		return this.getValue().compareTo(((BooleanParameter) o).getValue());
	}
}
