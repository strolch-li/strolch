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
package li.strolch.model.query;

import java.util.Date;
import java.util.List;

import ch.eitchnet.utils.StringMatchMode;
import ch.eitchnet.utils.collections.DateRange;
import ch.eitchnet.utils.dbc.DBC;
import ch.eitchnet.utils.iso8601.ISO8601FormatFactory;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public abstract class ParameterSelection implements Selection {

	private String bagKey;
	private String paramKey;

	public ParameterSelection(String bagKey, String paramKey) {
		this.bagKey = bagKey;
		this.paramKey = paramKey;
	}

	/**
	 * @return the bagKey
	 */
	public String getBagKey() {
		return this.bagKey;
	}

	/**
	 * @return the paramKey
	 */
	public String getParamKey() {
		return this.paramKey;
	}

	@Override
	public boolean hasSelection() {
		return true;
	}

	@Override
	public void accept(QueryVisitor visitor) {
		accept((ParameterSelectionVisitor) visitor);
	}

	@SuppressWarnings("nls")
	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append(getClass().getSimpleName() + " [bagKey=");
		builder.append(this.bagKey);
		builder.append(", paramKey=");
		builder.append(this.paramKey);
		builder.append("]");
		return builder.toString();
	}

	public abstract void accept(ParameterSelectionVisitor visitor);

	public static StringParameterSelection stringSelection(String bagKey, String paramKey, String value,
			StringMatchMode matchMode) {
		return new StringParameterSelection(bagKey, paramKey, value, matchMode);
	}

	public static IntegerParameterSelection integerSelection(String bagKey, String paramKey, int value) {
		return new IntegerParameterSelection(bagKey, paramKey, value);
	}

	public static BooleanParameterSelection booleanSelection(String bagKey, String paramKey, boolean value) {
		return new BooleanParameterSelection(bagKey, paramKey, value);
	}

	public static FloatParameterSelection floatSelection(String bagKey, String paramKey, double value) {
		return new FloatParameterSelection(bagKey, paramKey, value);
	}

	public static LongParameterSelection longSelection(String bagKey, String paramKey, long value) {
		return new LongParameterSelection(bagKey, paramKey, value);
	}

	public static DateParameterSelection dateSelection(String bagKey, String paramKey, Date value) {
		return new DateParameterSelection(bagKey, paramKey, value);
	}

	public static DurationParameterSelection durationSelection(String bagKey, String paramKey, String valueAsString) {
		return durationSelection(bagKey, paramKey,
				ISO8601FormatFactory.getInstance().getDurationFormat().parse(valueAsString));
	}

	public static DurationParameterSelection durationSelection(String bagKey, String paramKey, long value) {
		return new DurationParameterSelection(bagKey, paramKey, value);
	}

	public static DateRangeParameterSelection dateRangeSelection(String bagKey, String paramKey, DateRange dateRange) {
		return new DateRangeParameterSelection(bagKey, paramKey, dateRange);
	}

	public static StringListParameterSelection stringListSelection(String bagKey, String paramKey, List<String> value) {
		return new StringListParameterSelection(bagKey, paramKey, value);
	}

	public static IntegerListParameterSelection integerListSelection(String bagKey, String paramKey,
			List<Integer> value) {
		return new IntegerListParameterSelection(bagKey, paramKey, value);
	}

	public static FloatListParameterSelection floatListSelection(String bagKey, String paramKey, List<Double> value) {
		return new FloatListParameterSelection(bagKey, paramKey, value);
	}

	public static LongListParameterSelection longListSelection(String bagKey, String paramKey, List<Long> value) {
		return new LongListParameterSelection(bagKey, paramKey, value);
	}

	public static NullParameterSelection nullSelection(String bagKey, String paramKey) {
		return new NullParameterSelection(bagKey, paramKey);
	}

	public static AnyTypeParameterSelection anyTypeSelection(String bagKey, String paramKey, String value,
			StringMatchMode matchMode) {
		return new AnyTypeParameterSelection(bagKey, paramKey, value, matchMode);
	}

	public static class AnyTypeParameterSelection extends ParameterSelection {

		private StringMatchMode matchMode;
		private String value;

		public AnyTypeParameterSelection(String bagKey, String paramKey, String value, StringMatchMode matchMode) {
			super(bagKey, paramKey);
			this.value = value;
			this.matchMode = matchMode;
		}

		public String getValue() {
			return this.value;
		}

		public StringMatchMode getMatchMode() {
			return this.matchMode;
		}

		@Override
		public void accept(ParameterSelectionVisitor visitor) {
			visitor.visit(this);
		}
	}

	public static class NullParameterSelection extends ParameterSelection {

		public NullParameterSelection(String bagKey, String paramKey) {
			super(bagKey, paramKey);
		}

		@Override
		public void accept(ParameterSelectionVisitor visitor) {
			visitor.visit(this);
		}
	}

	public static class StringParameterSelection extends ParameterSelection {

		private StringMatchMode matchMode;
		private String value;

		public StringParameterSelection(String bagKey, String paramKey, String value, StringMatchMode matchMode) {
			super(bagKey, paramKey);
			this.value = value;
			this.matchMode = matchMode;
		}

		public String getValue() {
			return this.value;
		}

		public StringMatchMode getMatchMode() {
			return this.matchMode;
		}

		@Override
		public void accept(ParameterSelectionVisitor visitor) {
			visitor.visit(this);
		}
	}

	public static class IntegerParameterSelection extends ParameterSelection {

		private Integer value;

		public IntegerParameterSelection(String bagKey, String paramKey, Integer value) {
			super(bagKey, paramKey);
			this.value = value;
		}

		public Integer getValue() {
			return this.value;
		}

		@Override
		public void accept(ParameterSelectionVisitor visitor) {
			visitor.visit(this);
		}
	}

	public static class BooleanParameterSelection extends ParameterSelection {

		private Boolean value;

		public BooleanParameterSelection(String bagKey, String paramKey, Boolean value) {
			super(bagKey, paramKey);
			this.value = value;
		}

		public Boolean getValue() {
			return this.value;
		}

		@Override
		public void accept(ParameterSelectionVisitor visitor) {
			visitor.visit(this);
		}
	}

	public static class LongParameterSelection extends ParameterSelection {

		private Long value;

		public LongParameterSelection(String bagKey, String paramKey, Long value) {
			super(bagKey, paramKey);
			this.value = value;
		}

		public Long getValue() {
			return this.value;
		}

		@Override
		public void accept(ParameterSelectionVisitor visitor) {
			visitor.visit(this);
		}
	}

	public static class FloatParameterSelection extends ParameterSelection {

		private Double value;

		public FloatParameterSelection(String bagKey, String paramKey, Double value) {
			super(bagKey, paramKey);
			this.value = value;
		}

		public Double getValue() {
			return this.value;
		}

		@Override
		public void accept(ParameterSelectionVisitor visitor) {
			visitor.visit(this);
		}
	}

	public static class DateParameterSelection extends ParameterSelection {

		private Date value;

		public DateParameterSelection(String bagKey, String paramKey, Date value) {
			super(bagKey, paramKey);
			this.value = value;
		}

		public Date getValue() {
			return this.value;
		}

		@Override
		public void accept(ParameterSelectionVisitor visitor) {
			visitor.visit(this);
		}
	}

	public static class DateRangeParameterSelection extends ParameterSelection {

		private DateRange dateRange;

		public DateRangeParameterSelection(String bagKey, String paramKey, DateRange dateRange) {
			super(bagKey, paramKey);
			DBC.PRE.assertFalse("dateRange must be set!", dateRange == null); //$NON-NLS-1$
			this.dateRange = dateRange;
		}

		public DateRange getDateRange() {
			return this.dateRange;
		}

		@Override
		public void accept(ParameterSelectionVisitor visitor) {
			visitor.visit(this);
		}
	}

	public static class DurationParameterSelection extends ParameterSelection {

		private Long value;

		public DurationParameterSelection(String bagKey, String paramKey, Long value) {
			super(bagKey, paramKey);
			this.value = value;
		}

		public Long getValue() {
			return this.value;
		}

		@Override
		public void accept(ParameterSelectionVisitor visitor) {
			visitor.visit(this);
		}
	}

	public static abstract class AbstractListParameterSelection<T> extends ParameterSelection {

		private List<T> value;

		public AbstractListParameterSelection(String bagKey, String paramKey, List<T> value) {
			super(bagKey, paramKey);
			this.value = value;
		}

		public List<T> getValue() {
			return this.value;
		}

		@Override
		public abstract void accept(ParameterSelectionVisitor visitor);
	}

	public static class StringListParameterSelection extends AbstractListParameterSelection<String> {

		public StringListParameterSelection(String bagKey, String paramKey, List<String> value) {
			super(bagKey, paramKey, value);
		}

		@Override
		public void accept(ParameterSelectionVisitor visitor) {
			visitor.visit(this);
		}
	}

	public static class IntegerListParameterSelection extends AbstractListParameterSelection<Integer> {

		public IntegerListParameterSelection(String bagKey, String paramKey, List<Integer> value) {
			super(bagKey, paramKey, value);
		}

		@Override
		public void accept(ParameterSelectionVisitor visitor) {
			visitor.visit(this);
		}
	}

	public static class FloatListParameterSelection extends AbstractListParameterSelection<Double> {

		public FloatListParameterSelection(String bagKey, String paramKey, List<Double> value) {
			super(bagKey, paramKey, value);
		}

		@Override
		public void accept(ParameterSelectionVisitor visitor) {
			visitor.visit(this);
		}
	}

	public static class LongListParameterSelection extends AbstractListParameterSelection<Long> {

		public LongListParameterSelection(String bagKey, String paramKey, List<Long> value) {
			super(bagKey, paramKey, value);
		}

		@Override
		public void accept(ParameterSelectionVisitor visitor) {
			visitor.visit(this);
		}
	}
}
