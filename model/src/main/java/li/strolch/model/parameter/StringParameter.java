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

import java.text.MessageFormat;

import li.strolch.exception.StrolchException;
import li.strolch.model.StrolchValueType;
import li.strolch.model.visitor.StrolchElementVisitor;
import li.strolch.utils.dbc.DBC;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class StringParameter extends AbstractParameter<String> {

	protected String value = "";

	/**
	 * Empty constructor
	 */
	public StringParameter() {
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
	public StringParameter(String id, String name, String value) {
		super(id, name);
		setValue(value);
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
	public StringParameter(String id, String name, Enum<?> value) {
		super(id, name);
		setValueE(value);
	}

	@Override
	public String getType() {
		return StrolchValueType.STRING.getType();
	}

	@Override
	public StrolchValueType getValueType() {
		return StrolchValueType.STRING;
	}

	@SuppressWarnings("unchecked")
	@Override
	public String getValue() {
		return this.value;
	}

	@Override
	public String getValueAsString() {
		return this.value;
	}

	@Override
	public void setValue(String value) {
		assertNotReadonly();
		validateValue(value);
		this.value = value;
	}

	public void setValueE(Enum<?> value) {
		assertNotReadonly();
		if (value == null) {
			String msg = "Can not set null value on Parameter {0}";
			msg = MessageFormat.format(msg, getLocator());
			throw new StrolchException(msg);
		}
		validateValue(value.name());
		this.value = value.name();
	}

	@Override
	public void setValueFrom(Parameter<String> parameter) {
		assertNotReadonly();
		this.value = parameter.getValue();
	}

	/**
	 * Sets the value to the empty string
	 *
	 * @see Parameter#clear()
	 */
	@Override
	public void clear() {
		assertNotReadonly();
		this.value = "";
	}

	/**
	 * @return true if the string is empty
	 */
	@Override
	public boolean isEmpty() {
		return this.value.isEmpty();
	}

	@Override
	public boolean isEqualTo(Parameter<String> otherValue) {
		StringParameter other = (StringParameter) otherValue;
		return this.value.equals(other.value);
	}

	@Override
	public boolean isEqualTo(String otherValue) {
		return this.value.equals(otherValue);
	}

	@Override
	public void setValueFromString(String valueAsString) {
		setValue(valueAsString);
	}

	@Override
	public StringParameter getClone() {
		StringParameter clone = new StringParameter();
		super.fillClone(clone);
		clone.value = this.value;
		return clone;
	}

	@Override
	public <U> U accept(StrolchElementVisitor<U> visitor) {
		return visitor.visitStringParam(this);
	}

	@Override
	public int compareTo(Parameter<?> o) {
		DBC.PRE.assertEquals("Not same Parameter types!", this.getType(), o.getType());
		return this.getValue().compareToIgnoreCase(((StringParameter) o).getValue());
	}
}
