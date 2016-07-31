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

import li.strolch.model.ParameterBag;
import li.strolch.model.ParameterBagContainer;
import li.strolch.model.parameter.BooleanParameter;
import li.strolch.model.parameter.DateParameter;
import li.strolch.model.parameter.DurationParameter;
import li.strolch.model.parameter.FloatParameter;
import li.strolch.model.parameter.IntegerParameter;
import li.strolch.model.parameter.ListParameter;
import li.strolch.model.parameter.LongParameter;
import li.strolch.model.parameter.Parameter;
import li.strolch.model.parameter.StringParameter;
import li.strolch.utils.StringMatchMode;
import li.strolch.utils.collections.DateRange;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public abstract class ParameterSelector<T extends ParameterBagContainer> implements Selector<T> {

	protected String bagKey;
	protected String paramKey;

	public ParameterSelector(String bagKey, String key) {
		this.bagKey = bagKey;
		this.paramKey = key;
	}

	@Override
	public abstract boolean select(ParameterBagContainer element);

	public static <T extends ParameterBagContainer> StringParameterSelector<T> stringSelector(String bagKey,
			String paramKey, String value, StringMatchMode matchMode) {
		return new StringParameterSelector<>(bagKey, paramKey, value, matchMode);
	}

	public static <T extends ParameterBagContainer> IntegerParameterSelector<T> integerSelector(String bagKey,
			String paramKey, int value) {
		return new IntegerParameterSelector<>(bagKey, paramKey, value);
	}

	public static <T extends ParameterBagContainer> BooleanParameterSelector<T> booleanSelector(String bagKey,
			String paramKey, boolean value) {
		return new BooleanParameterSelector<>(bagKey, paramKey, value);
	}

	public static <T extends ParameterBagContainer> FloatParameterSelector<T> floatSelector(String bagKey,
			String paramKey, double value) {
		return new FloatParameterSelector<>(bagKey, paramKey, value);
	}

	public static <T extends ParameterBagContainer> LongParameterSelector<T> longSelector(String bagKey,
			String paramKey, long value) {
		return new LongParameterSelector<>(bagKey, paramKey, value);
	}

	public static <T extends ParameterBagContainer> DateParameterSelector<T> dateSelector(String bagKey,
			String paramKey, Date value) {
		return new DateParameterSelector<>(bagKey, paramKey, value);
	}

	public static <T extends ParameterBagContainer> DurationParameterSelector<T> durationSelector(String bagKey,
			String paramKey, Long value) {
		return new DurationParameterSelector<>(bagKey, paramKey, value);
	}

	public static <T extends ParameterBagContainer> DateRangeParameterSelector<T> dateRangeSelector(String bagKey,
			String paramKey, DateRange dateRange) {
		return new DateRangeParameterSelector<>(bagKey, paramKey, dateRange);
	}

	public static <T extends ParameterBagContainer> StringListParameterSelector<T> stringListSelector(String bagKey,
			String paramKey, List<String> value) {
		return new StringListParameterSelector<>(bagKey, paramKey, value);
	}

	public static <T extends ParameterBagContainer> IntegerListParameterSelector<T> integerListSelector(String bagKey,
			String paramKey, List<Integer> value) {
		return new IntegerListParameterSelector<>(bagKey, paramKey, value);
	}

	public static <T extends ParameterBagContainer> FloatListParameterSelector<T> floatListSelector(String bagKey,
			String paramKey, List<Double> value) {
		return new FloatListParameterSelector<>(bagKey, paramKey, value);
	}

	public static <T extends ParameterBagContainer> LongListParameterSelector<T> longListSelector(String bagKey,
			String paramKey, List<Long> value) {
		return new LongListParameterSelector<>(bagKey, paramKey, value);
	}

	public static <T extends ParameterBagContainer> NullParameterSelector<T> nullSelector(String bagKey,
			String paramKey) {
		return new NullParameterSelector<>(bagKey, paramKey);
	}

	public static <T extends ParameterBagContainer> AnyTypeParameterSelector<T> anyTypeSelection(String bagKey,
			String paramKey, String value, StringMatchMode matchMode) {
		return new AnyTypeParameterSelector<>(bagKey, paramKey, value, matchMode);
	}

	public static class AnyTypeParameterSelector<T extends ParameterBagContainer> extends ParameterSelector<T> {

		private StringMatchMode matchMode;
		private String value;

		public AnyTypeParameterSelector(String bagKey, String key, String value, StringMatchMode matchMode) {
			super(bagKey, key);
			this.value = value;
			this.matchMode = matchMode;
		}

		@Override
		public boolean select(ParameterBagContainer element) {

			ParameterBag bag = element.getParameterBag(this.bagKey);
			if (bag == null) {
				return false;
			}

			Parameter<?> parameter = bag.getParameter(this.paramKey);
			if (parameter == null)
				return false;

			String valueAsString = parameter.getValueAsString();
			return this.matchMode.matches(valueAsString, this.value);
		}
	}

	public static class NullParameterSelector<T extends ParameterBagContainer> extends ParameterSelector<T> {

		public NullParameterSelector(String bagKey, String key) {
			super(bagKey, key);
		}

		@Override
		public boolean select(ParameterBagContainer element) {

			ParameterBag bag = element.getParameterBag(this.bagKey);
			if (bag == null) {
				return false;
			}

			return !bag.hasParameter(this.paramKey);
		}
	}

	public static class StringParameterSelector<T extends ParameterBagContainer> extends ParameterSelector<T> {

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
		public boolean select(ParameterBagContainer element) {

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

	public static class IntegerParameterSelector<T extends ParameterBagContainer> extends ParameterSelector<T> {

		private Integer value;

		public IntegerParameterSelector(String bagKey, String paramKey, Integer value) {
			super(bagKey, paramKey);
			this.value = value;
		}

		@Override
		public boolean select(ParameterBagContainer element) {
			if (!element.hasParameterBag(this.bagKey))
				return false;

			ParameterBag bag = element.getParameterBag(this.bagKey);
			if (!bag.hasParameter(this.paramKey))
				return false;

			IntegerParameter param = bag.getParameter(this.paramKey);
			return param.getValue().equals(this.value);
		}
	}

	public static class BooleanParameterSelector<T extends ParameterBagContainer> extends ParameterSelector<T> {

		private Boolean value;

		public BooleanParameterSelector(String bagKey, String paramKey, Boolean value) {
			super(bagKey, paramKey);
			this.value = value;
		}

		@Override
		public boolean select(ParameterBagContainer element) {
			if (!element.hasParameterBag(this.bagKey))
				return false;

			ParameterBag bag = element.getParameterBag(this.bagKey);
			if (!bag.hasParameter(this.paramKey))
				return false;

			BooleanParameter param = bag.getParameter(this.paramKey);
			return param.getValue().equals(this.value);
		}
	}

	public static class FloatParameterSelector<T extends ParameterBagContainer> extends ParameterSelector<T> {

		private Double value;

		public FloatParameterSelector(String bagKey, String paramKey, Double value) {
			super(bagKey, paramKey);
			this.value = value;
		}

		@Override
		public boolean select(ParameterBagContainer element) {
			if (!element.hasParameterBag(this.bagKey))
				return false;

			ParameterBag bag = element.getParameterBag(this.bagKey);
			if (!bag.hasParameter(this.paramKey))
				return false;

			FloatParameter param = bag.getParameter(this.paramKey);
			return param.getValue().equals(this.value);
		}
	}

	public static class LongParameterSelector<T extends ParameterBagContainer> extends ParameterSelector<T> {

		private Long value;

		public LongParameterSelector(String bagKey, String paramKey, Long value) {
			super(bagKey, paramKey);
			this.value = value;
		}

		@Override
		public boolean select(ParameterBagContainer element) {
			if (!element.hasParameterBag(this.bagKey))
				return false;

			ParameterBag bag = element.getParameterBag(this.bagKey);
			if (!bag.hasParameter(this.paramKey))
				return false;

			LongParameter param = bag.getParameter(this.paramKey);
			return param.getValue().equals(this.value);
		}
	}

	public static class DateParameterSelector<T extends ParameterBagContainer> extends ParameterSelector<T> {

		private Date value;

		public DateParameterSelector(String bagKey, String paramKey, Date value) {
			super(bagKey, paramKey);
			this.value = value;
		}

		@Override
		public boolean select(ParameterBagContainer element) {
			if (!element.hasParameterBag(this.bagKey))
				return false;

			ParameterBag bag = element.getParameterBag(this.bagKey);
			if (!bag.hasParameter(this.paramKey))
				return false;

			DateParameter param = bag.getParameter(this.paramKey);
			return param.getValue().equals(this.value);
		}
	}

	public static class DateRangeParameterSelector<T extends ParameterBagContainer> extends ParameterSelector<T> {

		private DateRange dateRange;

		public DateRangeParameterSelector(String bagKey, String paramKey, DateRange dateRange) {
			super(bagKey, paramKey);
			this.dateRange = dateRange;
		}

		@Override
		public boolean select(ParameterBagContainer element) {
			if (!element.hasParameterBag(this.bagKey))
				return false;

			ParameterBag bag = element.getParameterBag(this.bagKey);
			if (!bag.hasParameter(this.paramKey))
				return false;

			DateParameter param = bag.getParameter(this.paramKey);
			Date value = param.getValue();

			return this.dateRange.contains(value);
		}
	}

	public static class DurationParameterSelector<T extends ParameterBagContainer> extends ParameterSelector<T> {

		private Long value;

		public DurationParameterSelector(String bagKey, String paramKey, Long value) {
			super(bagKey, paramKey);
			this.value = value;
		}

		@Override
		public boolean select(ParameterBagContainer element) {
			if (!element.hasParameterBag(this.bagKey))
				return false;

			ParameterBag bag = element.getParameterBag(this.bagKey);
			if (!bag.hasParameter(this.paramKey))
				return false;

			DurationParameter param = bag.getParameter(this.paramKey);
			return param.getValue().equals(this.value);
		}
	}

	public static abstract class AbstractListParameterSelector<U, T extends ParameterBagContainer>
			extends ParameterSelector<T> {

		private List<U> value;

		public AbstractListParameterSelector(String bagKey, String paramKey, List<U> value) {
			super(bagKey, paramKey);
			this.value = value;
		}

		@Override
		public boolean select(ParameterBagContainer element) {
			if (!element.hasParameterBag(this.bagKey))
				return false;

			ParameterBag bag = element.getParameterBag(this.bagKey);
			if (!bag.hasParameter(this.paramKey))
				return false;

			ListParameter<U> param = bag.getParameter(this.paramKey);
			return param.getValue().containsAll(this.value);
		}
	}

	public static class StringListParameterSelector<T extends ParameterBagContainer>
			extends AbstractListParameterSelector<String, T> {

		public StringListParameterSelector(String bagKey, String paramKey, List<String> value) {
			super(bagKey, paramKey, value);
		}
	}

	public static class IntegerListParameterSelector<T extends ParameterBagContainer>
			extends AbstractListParameterSelector<Integer, T> {

		public IntegerListParameterSelector(String bagKey, String paramKey, List<Integer> value) {
			super(bagKey, paramKey, value);
		}
	}

	public static class FloatListParameterSelector<T extends ParameterBagContainer>
			extends AbstractListParameterSelector<Double, T> {

		public FloatListParameterSelector(String bagKey, String paramKey, List<Double> value) {
			super(bagKey, paramKey, value);
		}
	}

	public static class LongListParameterSelector<T extends ParameterBagContainer>
			extends AbstractListParameterSelector<Long, T> {

		public LongListParameterSelector(String bagKey, String paramKey, List<Long> value) {
			super(bagKey, paramKey, value);
		}
	}
}
