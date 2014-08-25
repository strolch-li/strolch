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
package li.strolch.runtime.query.inmemory;

import java.util.Date;
import java.util.List;

import li.strolch.model.GroupedParameterizedElement;
import li.strolch.model.ParameterBag;
import li.strolch.model.parameter.BooleanParameter;
import li.strolch.model.parameter.DateParameter;
import li.strolch.model.parameter.FloatParameter;
import li.strolch.model.parameter.IntegerParameter;
import li.strolch.model.parameter.LongParameter;
import li.strolch.model.parameter.StringListParameter;
import li.strolch.model.parameter.StringParameter;
import ch.eitchnet.utils.StringMatchMode;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public abstract class ParameterSelector<T extends GroupedParameterizedElement> implements Selector<T> {

	protected String bagKey;
	protected String paramKey;

	public ParameterSelector(String bagKey, String key) {
		this.bagKey = bagKey;
		this.paramKey = key;
	}

	@Override
	public abstract boolean select(GroupedParameterizedElement element);

	public static <T extends GroupedParameterizedElement> StringParameterSelector<T> stringSelector(String bagKey,
			String paramKey, String value, StringMatchMode matchMode) {
		return new StringParameterSelector<>(bagKey, paramKey, value, matchMode);
	}

	public static <T extends GroupedParameterizedElement> IntegerParameterSelector<T> integerSelector(String bagKey,
			String paramKey, int value) {
		return new IntegerParameterSelector<>(bagKey, paramKey, value);
	}

	public static <T extends GroupedParameterizedElement> BooleanParameterSelector<T> booleanSelector(String bagKey,
			String paramKey, boolean value) {
		return new BooleanParameterSelector<>(bagKey, paramKey, value);
	}

	public static <T extends GroupedParameterizedElement> FloatParameterSelector<T> floatSelector(String bagKey,
			String paramKey, double value) {
		return new FloatParameterSelector<>(bagKey, paramKey, value);
	}

	public static <T extends GroupedParameterizedElement> LongParameterSelector<T> longSelector(String bagKey,
			String paramKey, long value) {
		return new LongParameterSelector<>(bagKey, paramKey, value);
	}

	public static <T extends GroupedParameterizedElement> DateParameterSelector<T> dateSelector(String bagKey,
			String paramKey, Date value) {
		return new DateParameterSelector<>(bagKey, paramKey, value);
	}

	public static <T extends GroupedParameterizedElement> DateRangeParameterSelector<T> dateRangeSelector(
			String bagKey, String paramKey, Date from, Date to) {
		return new DateRangeParameterSelector<>(bagKey, paramKey, from, to);
	}

	public static <T extends GroupedParameterizedElement> StringListParameterSelector<T> stringListSelector(
			String bagKey, String paramKey, List<String> value) {
		return new StringListParameterSelector<>(bagKey, paramKey, value);
	}

	public static class StringParameterSelector<T extends GroupedParameterizedElement> extends ParameterSelector<T> {

		private StringMatchMode matchMode;
		private String value;

		public StringParameterSelector(String bagKey, String paramKey, String value, StringMatchMode matchMode) {
			super(bagKey, paramKey);
			this.value = value;
			this.matchMode = matchMode;
		}

		public StringMatchMode getMatchMode() {
			return this.matchMode;
		}

		@Override
		public boolean select(GroupedParameterizedElement element) {

			if (!element.hasParameterBag(this.bagKey))
				return false;

			ParameterBag bag = element.getParameterBag(this.bagKey);
			if (!bag.hasParameter(this.paramKey))
				return false;

			StringParameter param = bag.getParameter(this.paramKey);
			String paramValue = param.getValue();

			return this.matchMode.matches(paramValue, this.value);
		}
	}

	public static class IntegerParameterSelector<T extends GroupedParameterizedElement> extends ParameterSelector<T> {

		private Integer value;

		public IntegerParameterSelector(String bagKey, String paramKey, Integer value) {
			super(bagKey, paramKey);
			this.value = value;
		}

		@Override
		public boolean select(GroupedParameterizedElement element) {
			if (!element.hasParameterBag(this.bagKey))
				return false;

			ParameterBag bag = element.getParameterBag(this.bagKey);
			if (!bag.hasParameter(this.paramKey))
				return false;

			IntegerParameter param = bag.getParameter(this.paramKey);
			return param.getValue().equals(this.value);
		}
	}

	public static class BooleanParameterSelector<T extends GroupedParameterizedElement> extends ParameterSelector<T> {

		private Boolean value;

		public BooleanParameterSelector(String bagKey, String paramKey, Boolean value) {
			super(bagKey, paramKey);
			this.value = value;
		}

		@Override
		public boolean select(GroupedParameterizedElement element) {
			if (!element.hasParameterBag(this.bagKey))
				return false;

			ParameterBag bag = element.getParameterBag(this.bagKey);
			if (!bag.hasParameter(this.paramKey))
				return false;

			BooleanParameter param = bag.getParameter(this.paramKey);
			return param.getValue().equals(this.value);
		}
	}

	public static class FloatParameterSelector<T extends GroupedParameterizedElement> extends ParameterSelector<T> {

		private Double value;

		public FloatParameterSelector(String bagKey, String paramKey, Double value) {
			super(bagKey, paramKey);
			this.value = value;
		}

		@Override
		public boolean select(GroupedParameterizedElement element) {
			if (!element.hasParameterBag(this.bagKey))
				return false;

			ParameterBag bag = element.getParameterBag(this.bagKey);
			if (!bag.hasParameter(this.paramKey))
				return false;

			FloatParameter param = bag.getParameter(this.paramKey);
			return param.getValue().equals(this.value);
		}
	}

	public static class LongParameterSelector<T extends GroupedParameterizedElement> extends ParameterSelector<T> {

		private Long value;

		public LongParameterSelector(String bagKey, String paramKey, Long value) {
			super(bagKey, paramKey);
			this.value = value;
		}

		@Override
		public boolean select(GroupedParameterizedElement element) {
			if (!element.hasParameterBag(this.bagKey))
				return false;

			ParameterBag bag = element.getParameterBag(this.bagKey);
			if (!bag.hasParameter(this.paramKey))
				return false;

			LongParameter param = bag.getParameter(this.paramKey);
			return param.getValue().equals(this.value);
		}
	}

	public static class DateParameterSelector<T extends GroupedParameterizedElement> extends ParameterSelector<T> {

		private Date value;

		public DateParameterSelector(String bagKey, String paramKey, Date value) {
			super(bagKey, paramKey);
			this.value = value;
		}

		@Override
		public boolean select(GroupedParameterizedElement element) {
			if (!element.hasParameterBag(this.bagKey))
				return false;

			ParameterBag bag = element.getParameterBag(this.bagKey);
			if (!bag.hasParameter(this.paramKey))
				return false;

			DateParameter param = bag.getParameter(this.paramKey);
			return param.getValue().equals(this.value);
		}
	}

	public static class DateRangeParameterSelector<T extends GroupedParameterizedElement> extends ParameterSelector<T> {

		private Date from;
		private Date to;

		public DateRangeParameterSelector(String bagKey, String paramKey, Date from, Date to) {
			super(bagKey, paramKey);
			this.from = from;
			this.to = to;
		}

		@Override
		public boolean select(GroupedParameterizedElement element) {
			if (!element.hasParameterBag(this.bagKey))
				return false;

			ParameterBag bag = element.getParameterBag(this.bagKey);
			if (!bag.hasParameter(this.paramKey))
				return false;

			DateParameter param = bag.getParameter(this.paramKey);
			Date value = param.getValue();

			return (this.from == null || !value.before(this.from)) && (this.to == null || !value.after(this.to));
		}
	}

	public static class StringListParameterSelector<T extends GroupedParameterizedElement> extends ParameterSelector<T> {

		private List<String> value;

		public StringListParameterSelector(String bagKey, String paramKey, List<String> value) {
			super(bagKey, paramKey);
			this.value = value;
		}

		@Override
		public boolean select(GroupedParameterizedElement element) {
			if (!element.hasParameterBag(this.bagKey))
				return false;

			ParameterBag bag = element.getParameterBag(this.bagKey);
			if (!bag.hasParameter(this.paramKey))
				return false;

			StringListParameter param = bag.getParameter(this.paramKey);
			return param.getValue().equals(this.value);
		}
	}
}
