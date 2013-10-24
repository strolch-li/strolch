package li.strolch.model.timevalue.impl;

import java.io.Serializable;

import li.strolch.model.timevalue.IValue;
import li.strolch.model.timevalue.IValueChange;

/**
 * @author Martin Smock <smock.martin@gmail.com>
 */
@SuppressWarnings("rawtypes")
public class ValueChange<T extends IValue> implements IValueChange<T>, Serializable {

	private static final long serialVersionUID = 1L;

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
		return this.time;
	}

	@Override
	@SuppressWarnings("unchecked")
	public T getValue() {
		return (T) this.value.getCopy();
	}

	@Override
	@SuppressWarnings("unchecked")
	public IValueChange<T> getInverse() {
		return new ValueChange(this.time, this.value.getInverse());
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
		if (this.time == null) {
			if (other.time != null)
				return false;
		} else if (!this.time.equals(other.time))
			return false;
		if (this.value == null) {
			if (other.value != null)
				return false;
		} else if (!this.value.equals(other.value))
			return false;
		return true;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((this.time == null) ? 0 : this.time.hashCode());
		result = prime * result + ((this.value == null) ? 0 : this.value.hashCode());
		return result;
	}

	@SuppressWarnings("nls")
	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append("ValueChange [time=");
		sb.append(this.time);
		sb.append(", value=");
		sb.append(this.value);
		sb.append("]");
		return sb.toString();
	}

}
