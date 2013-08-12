package li.strolch.model.timevalue.impl;

import java.io.Serializable;

import li.strolch.model.timevalue.ITimeValue;
import li.strolch.model.timevalue.IValue;

/**
 * {@link IValue} implementation to work with Double valued {@link ITimeValue} objects
 * 
 * @author Martin Smock <smock.martin@gmail.com>
 */
public class DoubleValue implements IValue<Double>, Serializable {

	private static final long serialVersionUID = 1L;

	public static final DoubleValue NEUTRAL = new DoubleValue(0.0d);

	private Double value;

	public DoubleValue(Double value) {
		this.value = value;
	}

	public DoubleValue(double value) {
		this.value = Double.valueOf(value);
	}

	public DoubleValue(Integer value) {
		this.value = this.value.doubleValue();
	}

	public DoubleValue(int value) {
		this.value = Integer.valueOf(value).doubleValue();
	}

	public DoubleValue(String valueAsString) throws NumberFormatException {
		this.value = Double.parseDouble(valueAsString);
	}

	@Override
	public DoubleValue add(Double o) {
		value += o;
		return this;
	}

	@Override
	public Double getValue() {
		return value;
	}

	@Override
	public String toString() {
		return "DoubleValue [value=" + value + "]";
	}

	@Override
	public boolean matches(IValue<Double> other) {
		return this.value.equals(other.getValue());
	}

	@Override
	public DoubleValue getInverse() {
		return new DoubleValue(-getValue());
	}

	@Override
	public DoubleValue getCopy(){
		return new DoubleValue(value); 
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
		DoubleValue other = (DoubleValue) obj;
		if (value == null) {
			if (other.value != null)
				return false;
		} else if (!value.equals(other.value))
			return false;
		return true;
	}
	

}
