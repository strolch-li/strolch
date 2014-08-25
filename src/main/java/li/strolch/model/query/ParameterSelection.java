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
import ch.eitchnet.utils.dbc.DBC;

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

	public static DateRangeParameterSelection dateRangeSelection(String bagKey, String paramKey, Date from, Date to) {
		return new DateRangeParameterSelection(bagKey, paramKey, from, to);
	}

	public static StringListParameterSelection stringListSelection(String bagKey, String paramKey, List<String> value) {
		return new StringListParameterSelection(bagKey, paramKey, value);
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

		private Date from;
		private Date to;

		public DateRangeParameterSelection(String bagKey, String paramKey, Date from, Date to) {
			super(bagKey, paramKey);
			DBC.PRE.assertFalse("Either 'to' or 'from' must be set! Both can not be null!", from == null && to == null); //$NON-NLS-1$
			this.from = from;
			this.to = to;
		}

		public Date getFrom() {
			return this.from;
		}

		public Date getTo() {
			return this.to;
		}

		@Override
		public void accept(ParameterSelectionVisitor visitor) {
			visitor.visit(this);
		}
	}

	public static class StringListParameterSelection extends ParameterSelection {

		private List<String> value;

		public StringListParameterSelection(String bagKey, String paramKey, List<String> value) {
			super(bagKey, paramKey);
			this.value = value;
		}

		public List<String> getValue() {
			return this.value;
		}

		@Override
		public void accept(ParameterSelectionVisitor visitor) {
			visitor.visit(this);
		}
	}
}
