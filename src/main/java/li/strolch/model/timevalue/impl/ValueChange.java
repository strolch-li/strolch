package li.strolch.model.timevalue.impl;

import li.strolch.model.timevalue.IValue;
import li.strolch.model.timevalue.IValueChange;

/**
 * @author martin_smock
 */
@SuppressWarnings("rawtypes")
public class ValueChange<T extends IValue> implements IValueChange<T> {

	protected final Long time;
	protected final T value;

	/**
	 * @param time
	 * @param value
	 */
	public ValueChange(final Long time, final T value) {
		this.time = time;
		this.value = value;
	}

	@Override
	public Long getTime() {
		return time;
	}

	@Override
	@SuppressWarnings("unchecked")
	public T getValue() {
		return (T) value.getCopy();
	}

	@Override
	@SuppressWarnings("unchecked")
	public IValueChange<T> getInverse() {
		return new ValueChange(time, value.getInverse());
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		ValueChange<?> other = (ValueChange<?>) obj;
		if (time == null) {
			if (other.time != null)
				return false;
		} else if (!time.equals(other.time))
			return false;
		if (value == null) {
			if (other.value != null)
				return false;
		} else if (!value.equals(other.value))
			return false;
		return true;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((time == null) ? 0 : time.hashCode());
		result = prime * result + ((value == null) ? 0 : value.hashCode());
		return result;
	}

	@Override
	public String toString() {
		return "ValueChange [time=" + time + ", value=" + value + "]";
	}

}
