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

import java.time.Duration;
import java.time.Period;

import li.strolch.model.StrolchValueType;
import li.strolch.model.visitor.StrolchElementVisitor;
import li.strolch.utils.dbc.DBC;
import li.strolch.utils.time.PeriodDuration;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class DurationParameter extends AbstractParameter<PeriodDuration> {

	private PeriodDuration value;

	/**
	 * Empty constructor
	 */
	public DurationParameter() {
		//
	}

	/**
	 * Default Constructor
	 *
	 * @param id
	 * 		the id
	 * @param name
	 * 		the name
	 * @param value
	 * 		the value
	 */
	public DurationParameter(String id, String name, PeriodDuration value) {
		super(id, name);
		setValue(value);
	}

	/**
	 * Default Constructor
	 *
	 * @param id
	 * 		the id
	 * @param name
	 * 		the name
	 * @param millis
	 * 		the value as milliseconds
	 */
	public DurationParameter(String id, String name, long millis) {
		super(id, name);
		setValue(PeriodDuration.of(Period.ZERO, Duration.ofMillis(millis)));
	}

	@Override
	public String getValueAsString() {
		return this.value.toString();
	}

	@SuppressWarnings("unchecked")
	@Override
	public PeriodDuration getValue() {
		return this.value;
	}

	@Override
	public void setValue(PeriodDuration value) {
		assertNotReadonly();
		validateValue(value);
		this.value = value;
	}

	public void setValueFrom(long millis) {
		assertNotReadonly();
		this.value = PeriodDuration.of(Period.ZERO, Duration.ofMillis(millis));
	}

	public void setValueFrom(Period value) {
		assertNotReadonly();
		this.value = PeriodDuration.of(value);
	}

	public void setValueFrom(Duration duration) {
		assertNotReadonly();
		this.value = PeriodDuration.of(duration);
	}

	@Override
	public void setValueFrom(Parameter<PeriodDuration> parameter) {
		assertNotReadonly();
		this.value = parameter.getValue();
	}

	/**
	 * Sets the value to 0
	 *
	 * @see Parameter#clear()
	 */
	@Override
	public void clear() {
		assertNotReadonly();
		this.value = PeriodDuration.ZERO;
	}

	@Override
	public boolean isEmpty() {
		return this.value.equals(PeriodDuration.ZERO);
	}

	@Override
	public boolean isEqualTo(Parameter<PeriodDuration> otherValue) {
		DurationParameter other = (DurationParameter) otherValue;
		return this.value.equals(other.value);
	}

	@Override
	public boolean isEqualTo(PeriodDuration otherValue) {
		return this.value.equals(otherValue);
	}

	public long toMillis() {
		return this.value.toMillis();
	}

	public Period getPeriod() {
		return this.value.getPeriod();
	}

	public Duration getDuration() {
		return this.value.getDuration();
	}

	@Override
	public void setValueFromString(String valueAsString) {
		setValue(parseFromString(valueAsString));
	}

	@Override
	public String getType() {
		return StrolchValueType.DURATION.getType();
	}

	@Override
	public StrolchValueType getValueType() {
		return StrolchValueType.DURATION;
	}

	@Override
	public DurationParameter getClone() {
		DurationParameter clone = new DurationParameter();
		super.fillClone(clone);
		clone.value = this.value;
		return clone;
	}

	@Override
	public <U> U accept(StrolchElementVisitor<U> visitor) {
		return visitor.visitDurationParam(this);
	}

	public static PeriodDuration parseFromString(String valueS) {
		return PeriodDuration.parse(valueS);
	}

	@Override
	public int compareTo(Parameter<?> o) {
		DBC.PRE.assertEquals("Not same Parameter types!", this.getType(), o.getType());
		return this.getValue().compareTo(((DurationParameter) o).getValue());
	}
}
