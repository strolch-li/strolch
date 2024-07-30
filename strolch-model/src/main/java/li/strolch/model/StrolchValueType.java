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

import static java.util.stream.Collectors.joining;

import java.text.MessageFormat;
import java.time.LocalDateTime;
import java.time.ZonedDateTime;
import java.util.Date;
import java.util.List;
import java.util.Objects;

import com.google.gson.JsonPrimitive;
import li.strolch.model.parameter.*;
import li.strolch.model.timedstate.*;
import li.strolch.model.timevalue.IValue;
import li.strolch.model.timevalue.IValueChange;
import li.strolch.model.timevalue.impl.*;
import li.strolch.utils.iso8601.ISO8601;
import li.strolch.utils.time.PeriodDuration;

public enum StrolchValueType {

	/**
	 * Can be used for:
	 * <ul>
	 * <li>{@link Parameter}</li>
	 * <li>{@link TimedState}</li>
	 * <li>{@link IValue}</li>
	 * <li>{@link IValueChange}</li>
	 * </ul>
	 */
	BOOLEAN("Boolean") {
		@Override
		public JsonPrimitive valueToJson(Object value) {
			return new JsonPrimitive(((Boolean) value));
		}

		@Override
		public Object parseValue(String value) {
			return BooleanParameter.parseFromString(value);
		}

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

		@Override
		public boolean isBoolean() {
			return true;
		}
	},

	/**
	 * Can be used for:
	 * <ul>
	 * <li>{@link Parameter}</li>
	 * <li>{@link TimedState}</li>
	 * <li>{@link IValue}</li>
	 * <li>{@link IValueChange}</li>
	 * </ul>
	 */
	INTEGER("Integer") {
		@Override
		public JsonPrimitive valueToJson(Object value) {
			return new JsonPrimitive(((Integer) value));
		}

		@Override
		public Object parseValue(String value) {
			return IntegerParameter.parseFromString(value);
		}

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

		@Override
		public boolean isNumber() {
			return true;
		}
	},

	/**
	 * Can be used for:
	 * <ul>
	 * <li>{@link Parameter}</li>
	 * <li>{@link TimedState}</li>
	 * <li>{@link IValue}</li>
	 * <li>{@link IValueChange}</li>
	 * </ul>
	 */
	FLOAT("Float") {
		@Override
		public JsonPrimitive valueToJson(Object value) {
			return new JsonPrimitive((Double) value);
		}

		@Override
		public Object parseValue(String value) {
			return FloatParameter.parseFromString(value);
		}

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

		@Override
		public boolean isNumber() {
			return true;
		}
	},

	/**
	 * Can be used for:
	 * <ul>
	 * <li>{@link Parameter}</li>
	 * </ul>
	 */
	LONG("Long") {
		@Override
		public JsonPrimitive valueToJson(Object value) {
			return new JsonPrimitive((Long) value);
		}

		@Override
		public Object parseValue(String value) {
			return LongParameter.parseFromString(value);
		}

		@Override
		public Parameter<?> parameterInstance() {
			return new LongParameter();
		}

		@Override
		public StrolchTimedState<? extends IValue<?>> timedStateInstance() {
			return new LongTimedState();
		}

		@Override
		public IValue<?> valueInstance(String valueAsString) {
			return new LongValue(valueAsString);
		}

		@Override
		public boolean isNumber() {
			return true;
		}
	},

	/**
	 * Can be used for:
	 * <ul>
	 * <li>{@link Parameter}</li>
	 * </ul>
	 */
	STRING("String") {
		@Override
		public JsonPrimitive valueToJson(Object value) {
			return new JsonPrimitive((String) value);
		}

		@Override
		public Object parseValue(String value) {
			return value;
		}

		@Override
		public Parameter<?> parameterInstance() {
			return new StringParameter();
		}

		@Override
		public StrolchTimedState<? extends IValue<?>> timedStateInstance() {
			throw new UnsupportedOperationException(
					MessageFormat.format("TimeStates of type {0} are not supported!", getType()));
		}

		@Override
		public IValue<?> valueInstance(String valueAsString) {
			throw new UnsupportedOperationException(
					MessageFormat.format("Parameters of type {0} are not supported!", getType()));
		}

		@Override
		public boolean isString() {
			return true;
		}
	},

	/**
	 * Can be used for:
	 * <ul>
	 * <li>{@link Parameter}</li>
	 * </ul>
	 */
	TEXT("Text") {
		@Override
		public JsonPrimitive valueToJson(Object value) {
			return new JsonPrimitive((String) value);
		}

		@Override
		public Object parseValue(String value) {
			return value;
		}

		@Override
		public Parameter<?> parameterInstance() {
			return new TextParameter();
		}

		@Override
		public StrolchTimedState<? extends IValue<?>> timedStateInstance() {
			throw new UnsupportedOperationException(
					MessageFormat.format("TimeStates of type {0} are not supported!", getType()));
		}

		@Override
		public IValue<?> valueInstance(String valueAsString) {
			throw new UnsupportedOperationException(
					MessageFormat.format("Parameters of type {0} are not supported!", getType()));
		}

		@Override
		public boolean isString() {
			return true;
		}
	},

	/**
	 * Can be used for:
	 * <ul>
	 * <li>{@link Parameter}</li>
	 * </ul>
	 */
	DATE("Date") {
		@Override
		public JsonPrimitive valueToJson(Object value) {
			if (value instanceof Date)
				return new JsonPrimitive(ISO8601.toString((Date) value));
			else if (value instanceof LocalDateTime)
				return new JsonPrimitive(ISO8601.toString((LocalDateTime) value));
			if (value instanceof ZonedDateTime)
				return new JsonPrimitive(ISO8601.toString((ZonedDateTime) value));
			throw new ClassCastException("value " + value + " + has unexpected class " + value.getClass());
		}

		@Override
		public Object parseValue(String value) {
			return DateParameter.parseFromString(value);
		}

		@Override
		public Parameter<?> parameterInstance() {
			return new DateParameter();
		}

		@Override
		public StrolchTimedState<? extends IValue<?>> timedStateInstance() {
			throw new UnsupportedOperationException(
					MessageFormat.format("TimeStates of type {0} are not supported!", getType()));
		}

		@Override
		public IValue<?> valueInstance(String valueAsString) {
			throw new UnsupportedOperationException(
					MessageFormat.format("Parameters of type {0} are not supported!", getType()));
		}
	},

	/**
	 * Can be used for:
	 * <ul>
	 * <li>{@link Parameter}</li>
	 * </ul>
	 */
	DURATION("Duration") {
		@Override
		public JsonPrimitive valueToJson(Object value) {
			return new JsonPrimitive(((PeriodDuration) value).toString());
		}

		@Override
		public Object parseValue(String value) {
			return DurationParameter.parseFromString(value);
		}

		@Override
		public Parameter<?> parameterInstance() {
			return new DurationParameter();
		}

		@Override
		public StrolchTimedState<? extends IValue<?>> timedStateInstance() {
			throw new UnsupportedOperationException(
					MessageFormat.format("TimeStates of type {0} are not supported!", getType()));
		}

		@Override
		public IValue<?> valueInstance(String valueAsString) {
			throw new UnsupportedOperationException(
					MessageFormat.format("Parameters of type {0} are not supported!", getType()));
		}
	},

	/**
	 * Can be used for:
	 * <ul>
	 * <li>{@link Parameter}</li>
	 * <li>{@link TimedState}</li>
	 * <li>{@link IValue}</li>
	 * <li>{@link IValueChange}</li>
	 * </ul>
	 */
	FLOAT_LIST("FloatList") {
		@Override
		public JsonPrimitive valueToJson(Object value) {
			@SuppressWarnings("unchecked")
			List<Double> list = (List<Double>) value;
			return new JsonPrimitive(list.stream().map(Objects::toString).collect(joining(", ")));
		}

		@Override
		public Object parseValue(String value) {
			return FloatListParameter.parseFromString(value);
		}

		@Override
		public Parameter<?> parameterInstance() {
			return new FloatListParameter();
		}

		@Override
		public StrolchTimedState<? extends IValue<?>> timedStateInstance() {
			return new FloatListTimedState();
		}

		@Override
		public IValue<?> valueInstance(String valueAsString) {
			return new FloatListValue(valueAsString);
		}

		@Override
		public boolean isList() {
			return true;
		}
	},

	/**
	 * Can be used for:
	 * <ul>
	 * <li>{@link Parameter}</li>
	 * </ul>
	 */
	INTEGER_LIST("IntegerList") {
		@Override
		public JsonPrimitive valueToJson(Object value) {
			@SuppressWarnings("unchecked")
			List<Integer> list = (List<Integer>) value;
			return new JsonPrimitive(list.stream().map(Objects::toString).collect(joining(", ")));
		}

		@Override
		public Object parseValue(String value) {
			return IntegerListParameter.parseFromString(value);
		}

		@Override
		public Parameter<?> parameterInstance() {
			return new IntegerListParameter();
		}

		@Override
		public StrolchTimedState<? extends IValue<?>> timedStateInstance() {
			return new IntegerListTimedState();
		}

		@Override
		public IValue<?> valueInstance(String valueAsString) {
			return new IntegerListValue(valueAsString);
		}

		@Override
		public boolean isList() {
			return true;
		}
	},

	/**
	 * Can be used for:
	 * <ul>
	 * <li>{@link Parameter}</li>
	 * </ul>
	 */
	LONG_LIST("LongList") {
		@Override
		public JsonPrimitive valueToJson(Object value) {
			@SuppressWarnings("unchecked")
			List<Long> list = (List<Long>) value;
			return new JsonPrimitive(list.stream().map(Objects::toString).collect(joining(", ")));
		}

		@Override
		public Object parseValue(String value) {
			return LongListParameter.parseFromString(value);
		}

		@Override
		public Parameter<?> parameterInstance() {
			return new LongListParameter();
		}

		@Override
		public StrolchTimedState<? extends IValue<?>> timedStateInstance() {
			throw new UnsupportedOperationException(
					MessageFormat.format("TimeStates of type {0} are not supported!", getType()));
		}

		@Override
		public IValue<?> valueInstance(String valueAsString) {
			throw new UnsupportedOperationException(
					MessageFormat.format("Parameters of type {0} are not supported!", getType()));
		}

		@Override
		public boolean isList() {
			return true;
		}
	},

	/**
	 * Can be used for:
	 * <ul>
	 * <li>{@link Parameter}</li>
	 * </ul>
	 */
	STRING_LIST("StringList") {
		@Override
		public JsonPrimitive valueToJson(Object value) {
			@SuppressWarnings("unchecked")
			List<String> list = (List<String>) value;
			return new JsonPrimitive(list.stream().map(Objects::toString).collect(joining(", ")));
		}

		@Override
		public Object parseValue(String value) {
			return StringListParameter.parseFromString(value);
		}

		@Override
		public Parameter<?> parameterInstance() {
			return new StringListParameter();
		}

		@Override
		public StrolchTimedState<? extends IValue<?>> timedStateInstance() {
			throw new UnsupportedOperationException(
					MessageFormat.format("TimeStates of type {0} are not supported!", getType()));
		}

		@Override
		public IValue<?> valueInstance(String valueAsString) {
			throw new UnsupportedOperationException(
					MessageFormat.format("Values of type {0} are not supported!", getType()));
		}

		@Override
		public boolean isList() {
			return true;
		}
	},

	/**
	 * Can be used for:
	 * <ul>
	 * <li>{@link TimedState}</li>
	 * <li>{@link IValue}</li>
	 * <li>{@link IValueChange}</li>
	 * </ul>
	 */
	STRING_SET("StringSet") {
		@Override
		public JsonPrimitive valueToJson(Object value) {
			throw new UnsupportedOperationException(
					MessageFormat.format("Formatting of type {0} is not supported!", getType()));
		}

		@Override
		public Object parseValue(String value) {
			throw new UnsupportedOperationException(
					MessageFormat.format("Parsing value of type {0} is not supported!", getType()));
		}

		@Override
		public Parameter<?> parameterInstance() {
			throw new UnsupportedOperationException(
					MessageFormat.format("Parameters of type {0} are not supported!", getType()));
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

	private final String type;

	StrolchValueType(String type) {
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

	public abstract JsonPrimitive valueToJson(Object value);

	public abstract Object parseValue(String value);

	public abstract Parameter<?> parameterInstance();

	public abstract StrolchTimedState<? extends IValue<?>> timedStateInstance();

	public abstract IValue<?> valueInstance(String valueAsString);

	public boolean isNumber() {
		return false;
	}

	public boolean isBoolean() {
		return false;
	}

	public boolean isString() {
		return false;
	}

	public boolean isList() {
		return false;
	}
}