/*
 * Copyright (c) 2012, Robert von Burg
 *
 * All rights reserved.
 *
 * This file is part of the XXX.
 *
 *  XXX is free software: you can redistribute 
 *  it and/or modify it under the terms of the GNU General Public License as 
 *  published by the Free Software Foundation, either version 3 of the License, 
 *  or (at your option) any later version.
 *
 *  XXX is distributed in the hope that it will 
 *  be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with XXX.  If not, see 
 *  <http://www.gnu.org/licenses/>.
 */
package li.strolch.model.query;

import java.util.Date;
import java.util.List;

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
	public void accept(QueryVisitor visitor) {
		accept((ParameterSelectionVisitor) visitor);
	}

	public abstract void accept(ParameterSelectionVisitor visitor);

	public static ParameterSelection stringSelection(String bagKey, String paramKey, String value) {
		return new StringParameterSelection(bagKey, paramKey, value);
	}

	public static ParameterSelection integerSelection(String bagKey, String paramKey, int value) {
		return new IntegerParameterSelection(bagKey, paramKey, value);
	}

	public static ParameterSelection booleanSelection(String bagKey, String paramKey, boolean value) {
		return new BooleanParameterSelection(bagKey, paramKey, value);
	}

	public static ParameterSelection floatSelection(String bagKey, String paramKey, double value) {
		return new FloatParameterSelection(bagKey, paramKey, value);
	}

	public static ParameterSelection longSelection(String bagKey, String paramKey, long value) {
		return new LongParameterSelection(bagKey, paramKey, value);
	}

	public static ParameterSelection dateSelection(String bagKey, String paramKey, Date value) {
		return new DateParameterSelection(bagKey, paramKey, value);
	}

	public static ParameterSelection stringListSelection(String bagKey, String paramKey, List<String> value) {
		return new StringListParameterSelection(bagKey, paramKey, value);
	}

	public static class StringParameterSelection extends ParameterSelection {

		private String value;

		public StringParameterSelection(String bagKey, String paramKey, String value) {
			super(bagKey, paramKey);
			this.value = value;
		}

		public String getValue() {
			return this.value;
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
