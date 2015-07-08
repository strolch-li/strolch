/*
 * Copyright 2015 Robert von Burg <eitch@eitchnet.ch>
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
package li.strolch.model;

import java.text.MessageFormat;

import li.strolch.model.parameter.BooleanParameter;
import li.strolch.model.parameter.DateParameter;
import li.strolch.model.parameter.DurationParameter;
import li.strolch.model.parameter.FloatListParameter;
import li.strolch.model.parameter.FloatParameter;
import li.strolch.model.parameter.IntegerListParameter;
import li.strolch.model.parameter.IntegerParameter;
import li.strolch.model.parameter.LongListParameter;
import li.strolch.model.parameter.LongParameter;
import li.strolch.model.parameter.Parameter;
import li.strolch.model.parameter.StringListParameter;
import li.strolch.model.parameter.StringParameter;
import li.strolch.model.timedstate.BooleanTimedState;
import li.strolch.model.timedstate.FloatTimedState;
import li.strolch.model.timedstate.IntegerTimedState;
import li.strolch.model.timedstate.StringSetTimedState;
import li.strolch.model.timedstate.StrolchTimedState;
import li.strolch.model.timedstate.TimedState;
import li.strolch.model.timevalue.IValue;
import li.strolch.model.timevalue.IValueChange;
import li.strolch.model.timevalue.impl.BooleanValue;
import li.strolch.model.timevalue.impl.FloatValue;
import li.strolch.model.timevalue.impl.IntegerValue;
import li.strolch.model.timevalue.impl.StringSetValue;

public enum StrolchValueType {

	/**
	 * Can be used for:<br />
	 * <ul>
	 * <li>{@link Parameter}</li>
	 * <li>{@link TimedState}</li>
	 * <li>{@link IValue}</li>
	 * <li>{@link IValueChange}</li>
	 * </ul>
	 */
	BOOLEAN("Boolean") {
		@Override
		public Parameter<?> parameterInstance() {
			return new BooleanParameter();
		}

		@Override
		public StrolchTimedState<? extends IValue<?>> timedStateInstance() {
			return new BooleanTimedState();
		}

		@Override
		public IValue<?> valueInstance(String valueAsString) {
			return new BooleanValue(valueAsString);
		}
	},

	/**
	 * Can be used for:<br />
	 * <ul>
	 * <li>{@link Parameter}</li>
	 * <li>{@link TimedState}</li>
	 * <li>{@link IValue}</li>
	 * <li>{@link IValueChange}</li>
	 * </ul>
	 */
	INTEGER("Integer") {
		@Override
		public Parameter<?> parameterInstance() {
			return new IntegerParameter();
		}

		@Override
		public StrolchTimedState<? extends IValue<?>> timedStateInstance() {
			return new IntegerTimedState();
		}

		@Override
		public IValue<?> valueInstance(String valueAsString) {
			return new IntegerValue(valueAsString);
		}
	},

	/**
	 * Can be used for:<br />
	 * <ul>
	 * <li>{@link Parameter}</li>
	 * <li>{@link TimedState}</li>
	 * <li>{@link IValue}</li>
	 * <li>{@link IValueChange}</li>
	 * </ul>
	 */
	FLOAT("Float") {
		@Override
		public Parameter<?> parameterInstance() {
			return new FloatParameter();
		}

		@Override
		public StrolchTimedState<? extends IValue<?>> timedStateInstance() {
			return new FloatTimedState();
		}

		@Override
		public IValue<?> valueInstance(String valueAsString) {
			return new FloatValue(valueAsString);
		}
	},

	/**
	 * Can be used for:<br />
	 * <ul>
	 * <li>{@link Parameter}</li>
	 * </ul>
	 */
	LONG("Long") {
		@Override
		public Parameter<?> parameterInstance() {
			return new LongParameter();
		}

		@Override
		public StrolchTimedState<? extends IValue<?>> timedStateInstance() {
			throw new UnsupportedOperationException(MessageFormat.format(
					"TimeStates of type {0} are not supported!", getType())); //$NON-NLS-1$
		}

		@Override
		public IValue<?> valueInstance(String valueAsString) {
			throw new UnsupportedOperationException(MessageFormat.format(
					"Parameters of type {0} are not supported!", getType())); //$NON-NLS-1$
		}
	},

	/**
	 * Can be used for:<br />
	 * <ul>
	 * <li>{@link Parameter}</li>
	 * </ul>
	 */
	STRING("String") {
		@Override
		public Parameter<?> parameterInstance() {
			return new StringParameter();
		}

		@Override
		public StrolchTimedState<? extends IValue<?>> timedStateInstance() {
			throw new UnsupportedOperationException(MessageFormat.format(
					"TimeStates of type {0} are not supported!", getType())); //$NON-NLS-1$
		}

		@Override
		public IValue<?> valueInstance(String valueAsString) {
			throw new UnsupportedOperationException(MessageFormat.format(
					"Parameters of type {0} are not supported!", getType())); //$NON-NLS-1$
		}
	},

	/**
	 * Can be used for:<br />
	 * <ul>
	 * <li>{@link Parameter}</li>
	 * </ul>
	 */
	DATE("Date") {
		@Override
		public Parameter<?> parameterInstance() {
			return new DateParameter();
		}

		@Override
		public StrolchTimedState<? extends IValue<?>> timedStateInstance() {
			throw new UnsupportedOperationException(MessageFormat.format(
					"TimeStates of type {0} are not supported!", getType())); //$NON-NLS-1$
		}

		@Override
		public IValue<?> valueInstance(String valueAsString) {
			throw new UnsupportedOperationException(MessageFormat.format(
					"Parameters of type {0} are not supported!", getType())); //$NON-NLS-1$
		}
	},

	/**
	 * Can be used for:<br />
	 * <ul>
	 * <li>{@link Parameter}</li>
	 * </ul>
	 */
	DURATION("Duration") {
		@Override
		public Parameter<?> parameterInstance() {
			return new DurationParameter();
		}

		@Override
		public StrolchTimedState<? extends IValue<?>> timedStateInstance() {
			throw new UnsupportedOperationException(MessageFormat.format(
					"TimeStates of type {0} are not supported!", getType())); //$NON-NLS-1$
		}

		@Override
		public IValue<?> valueInstance(String valueAsString) {
			throw new UnsupportedOperationException(MessageFormat.format(
					"Parameters of type {0} are not supported!", getType())); //$NON-NLS-1$
		}
	},

	/**
	 * Can be used for:<br />
	 * <ul>
	 * <li>{@link Parameter}</li>
	 * </ul>
	 */
	FLOAT_LIST("FloatList") {
		@Override
		public Parameter<?> parameterInstance() {
			return new FloatListParameter();
		}

		@Override
		public StrolchTimedState<? extends IValue<?>> timedStateInstance() {
			throw new UnsupportedOperationException(MessageFormat.format(
					"TimeStates of type {0} are not supported!", getType())); //$NON-NLS-1$
		}

		@Override
		public IValue<?> valueInstance(String valueAsString) {
			throw new UnsupportedOperationException(MessageFormat.format(
					"Parameters of type {0} are not supported!", getType())); //$NON-NLS-1$
		}
	},

	/**
	 * Can be used for:<br />
	 * <ul>
	 * <li>{@link Parameter}</li>
	 * </ul>
	 */
	INTEGER_LIST("IntegerList") {
		@Override
		public Parameter<?> parameterInstance() {
			return new IntegerListParameter();
		}

		@Override
		public StrolchTimedState<? extends IValue<?>> timedStateInstance() {
			throw new UnsupportedOperationException(MessageFormat.format(
					"TimeStates of type {0} are not supported!", getType())); //$NON-NLS-1$
		}

		@Override
		public IValue<?> valueInstance(String valueAsString) {
			throw new UnsupportedOperationException(MessageFormat.format(
					"Parameters of type {0} are not supported!", getType())); //$NON-NLS-1$
		}
	},

	/**
	 * Can be used for:<br />
	 * <ul>
	 * <li>{@link Parameter}</li>
	 * </ul>
	 */
	LONG_LIST("LongList") {
		@Override
		public Parameter<?> parameterInstance() {
			return new LongListParameter();
		}

		@Override
		public StrolchTimedState<? extends IValue<?>> timedStateInstance() {
			throw new UnsupportedOperationException(MessageFormat.format(
					"TimeStates of type {0} are not supported!", getType())); //$NON-NLS-1$
		}

		@Override
		public IValue<?> valueInstance(String valueAsString) {
			throw new UnsupportedOperationException(MessageFormat.format(
					"Parameters of type {0} are not supported!", getType())); //$NON-NLS-1$
		}
	},

	/**
	 * Can be used for:<br />
	 * <ul>
	 * <li>{@link Parameter}</li>
	 * </ul>
	 */
	STRING_LIST("StringList") {
		@Override
		public Parameter<?> parameterInstance() {
			return new StringListParameter();
		}

		@Override
		public StrolchTimedState<? extends IValue<?>> timedStateInstance() {
			throw new UnsupportedOperationException(MessageFormat.format(
					"TimeStates of type {0} are not supported!", getType())); //$NON-NLS-1$
		}

		@Override
		public IValue<?> valueInstance(String valueAsString) {
			throw new UnsupportedOperationException(MessageFormat.format(
					"Values of type {0} are not supported!", getType())); //$NON-NLS-1$
		}
	},

	/**
	 * Can be used for:<br />
	 * <ul>
	 * <li>{@link TimedState}</li>
	 * <li>{@link IValue}</li>
	 * <li>{@link IValueChange}</li>
	 * </ul>
	 */
	STRING_SET("StringSet") {
		@Override
		public Parameter<?> parameterInstance() {
			throw new UnsupportedOperationException(MessageFormat.format(
					"Parameters of type {0} are not supported!", getType())); //$NON-NLS-1$
		}

		@Override
		public StrolchTimedState<? extends IValue<?>> timedStateInstance() {
			return new StringSetTimedState();
		}

		@Override
		public IValue<?> valueInstance(String valueAsString) {
			return new StringSetValue(valueAsString);
		}
	};

	private String type;

	private StrolchValueType(String type) {
		this.type = type;
	}

	public String getType() {
		return this.type;
	}

	public static StrolchValueType parse(String value) {

		// TODO this is for backwards compatibility where we still had States of type BooleanState instead of Boolean
		String strippedValue = value.replace("State", "");

		for (StrolchValueType type : StrolchValueType.values()) {
			if (type.type.equals(strippedValue))
				return type;
		}
		throw new IllegalArgumentException("Type " + value + " does not exist!");
	}

	public abstract Parameter<?> parameterInstance();

	public abstract StrolchTimedState<? extends IValue<?>> timedStateInstance();

	public abstract IValue<?> valueInstance(String valueAsString);
}