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
package li.strolch.runtime.query.inmemory;

import java.util.Date;
import java.util.List;

import li.strolch.model.ParameterizedElement;
import li.strolch.model.parameter.BooleanParameter;
import li.strolch.model.parameter.DateParameter;
import li.strolch.model.parameter.FloatParameter;
import li.strolch.model.parameter.IntegerParameter;
import li.strolch.model.parameter.LongParameter;
import li.strolch.model.parameter.StringListParameter;
import li.strolch.model.parameter.StringParameter;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public abstract class ParameterSelector implements Selector<ParameterizedElement> {

	protected String key;

	public ParameterSelector(String key) {
		this.key = key;
	}

	@Override
	public abstract boolean select(ParameterizedElement element);

	public static ParameterSelector stringSelector(String key, String value) {
		return new StringParameterSelector(key, value);
	}

	public static ParameterSelector integerSelector(String key, int value) {
		return new IntegerParameterSelector(key, value);
	}

	public static ParameterSelector booleanSelector(String key, boolean value) {
		return new BooleanParameterSelector(key, value);
	}

	public static ParameterSelector floatSelector(String key, double value) {
		return new FloatParameterSelector(key, value);
	}

	public static ParameterSelector longSelector(String key, long value) {
		return new LongParameterSelector(key, value);
	}

	public static ParameterSelector dateSelector(String key, Date value) {
		return new DateParameterSelector(key, value);
	}

	public static ParameterSelector stringListSelector(String key, List<String> value) {
		return new StringListParameterSelector(key, value);
	}

	private static class StringParameterSelector extends ParameterSelector {

		private String value;

		public StringParameterSelector(String key, String value) {
			super(key);
			this.value = value;
		}

		@Override
		public boolean select(ParameterizedElement element) {
			if (!element.hasParameter(this.key))
				return false;

			StringParameter param = element.getParameter(this.key);
			return param.getValue().equals(this.value);
		}
	}

	private static class IntegerParameterSelector extends ParameterSelector {

		private Integer value;

		public IntegerParameterSelector(String key, Integer value) {
			super(key);
			this.value = value;
		}

		@Override
		public boolean select(ParameterizedElement element) {
			if (!element.hasParameter(this.key))
				return false;

			IntegerParameter param = element.getParameter(this.key);
			return param.getValue().equals(this.value);
		}
	}

	private static class BooleanParameterSelector extends ParameterSelector {

		private Boolean value;

		public BooleanParameterSelector(String key, Boolean value) {
			super(key);
			this.value = value;
		}

		@Override
		public boolean select(ParameterizedElement element) {
			if (!element.hasParameter(this.key))
				return false;

			BooleanParameter param = element.getParameter(this.key);
			return param.getValue().equals(this.value);
		}
	}

	private static class FloatParameterSelector extends ParameterSelector {

		private Double value;

		public FloatParameterSelector(String key, Double value) {
			super(key);
			this.value = value;
		}

		@Override
		public boolean select(ParameterizedElement element) {
			if (!element.hasParameter(this.key))
				return false;

			FloatParameter param = element.getParameter(this.key);
			return param.getValue().equals(this.value);
		}
	}

	private static class LongParameterSelector extends ParameterSelector {

		private Long value;

		public LongParameterSelector(String key, Long value) {
			super(key);
			this.value = value;
		}

		@Override
		public boolean select(ParameterizedElement element) {
			if (!element.hasParameter(this.key))
				return false;

			LongParameter param = element.getParameter(this.key);
			return param.getValue().equals(this.value);
		}
	}

	private static class DateParameterSelector extends ParameterSelector {

		private Date value;

		public DateParameterSelector(String key, Date value) {
			super(key);
			this.value = value;
		}

		@Override
		public boolean select(ParameterizedElement element) {
			if (!element.hasParameter(this.key))
				return false;

			DateParameter param = element.getParameter(this.key);
			return param.getValue().equals(this.value);
		}
	}

	private static class StringListParameterSelector extends ParameterSelector {

		private List<String> value;

		public StringListParameterSelector(String key, List<String> value) {
			super(key);
			this.value = value;
		}

		@Override
		public boolean select(ParameterizedElement element) {
			if (!element.hasParameter(this.key))
				return false;

			StringListParameter param = element.getParameter(this.key);
			return param.getValue().equals(this.value);
		}
	}
}
