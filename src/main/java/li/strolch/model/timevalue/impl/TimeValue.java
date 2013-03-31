package li.strolch.model.timevalue.impl;

import java.io.Serializable;

import li.strolch.model.timevalue.ITimeValue;
import li.strolch.model.timevalue.IValue;

/**
 * @author Martin Smock <smock.martin@gmail.com>
 */
@SuppressWarnings("rawtypes")
public class TimeValue<T extends IValue> implements ITimeValue <T>, Serializable {
	
	private static final long serialVersionUID = 1L;
	
	protected final Long time; 
	protected T value; 
	
	/**
	 * @param time
	 * @param value
	 */
	public TimeValue(final Long time, final T value){
		this.time = time; 
		this.value = value; 
	}

	@Override
	@SuppressWarnings("unchecked")
	public T getValue() {
		return (T) value.getCopy();
	}

	@Override
	public Long getTime() {
		return time;
	}

	@Override
	public ITimeValue<T> setValue(final T value) {
		this.value = value;
		return this; 
	}

	@SuppressWarnings("unchecked")
	@Override
	public ITimeValue<T> add(final T change) {
		this.value.add(change.getValue()); 
		return this; 
	}
	
	@Override
	public int compareTo(final ITimeValue<T> arg0) {
		return this.getTime().compareTo(arg0.getTime());
	}

	@Override
	public String toString() {
		return "TimeValue [time=" + time + ", value=" + value + "]";
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
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		@SuppressWarnings("unchecked")
		TimeValue<T> other = (TimeValue<T>) obj;
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
	
}
