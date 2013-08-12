package li.strolch.model.timevalue.impl;

import java.io.Serializable;

import li.strolch.model.timevalue.ITimeValue;
import li.strolch.model.timevalue.IValue;

/**
 * {@link IValue} implementation to work with Integer valued {@link ITimeValue}
 * objects
 * 
 * @author Martin Smock <smock.martin@gmail.com>
 */
public class IntegerValue implements IValue<Integer>, Serializable {

	private static final long serialVersionUID = 1L;

	public static final IntegerValue NEUTRAL = new IntegerValue(0);

	private Integer value;

	public IntegerValue(Integer value) {
		this.value = value;
	}

	public IntegerValue(int value) {
		this.value = Integer.valueOf(value);
	}

	public IntegerValue(String valueAsString) throws NumberFormatException {
		this.value = Integer.parseInt(valueAsString);
	}

	@Override
	public IntegerValue add(Integer o) {
		value += o;
		return this;
	}

	@Override
	public boolean matches(IValue<Integer> other) {
		return this.value.equals(other.getValue());
	}

	@Override
	public Integer getValue() {
		return value;
	}

	@Override
	public IntegerValue getInverse() {
		return new IntegerValue(-getValue());
	}

	@Override
	public String toString() {
		return "IntegerValue [value=" + value + "]";
	}

	@Override
	public IntegerValue getCopy() {
		return new IntegerValue(value);
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((value == null) ? 0 : value.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		IntegerValue other = (IntegerValue) obj;
		if (value == null) {
			if (other.value != null)
				return false;
		} else if (!value.equals(other.value))
			return false;
		return true;
	}

}
