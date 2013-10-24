package li.strolch.model.timevalue.impl;

import java.io.Serializable;
import java.util.Collection;
import java.util.Iterator;
import java.util.SortedSet;
import java.util.TreeSet;

import li.strolch.model.timevalue.ITimeValue;
import li.strolch.model.timevalue.ITimeVariable;
import li.strolch.model.timevalue.IValue;
import li.strolch.model.timevalue.IValueChange;


/**
 * @author Martin Smock <smock.martin@gmail.com>
 */
@SuppressWarnings("rawtypes")
public class TimeVariable<T extends IValue> implements ITimeVariable<T>, Serializable {

	private static final long serialVersionUID = 1L;
	
	public SortedSet<ITimeValue<T>> container = new TreeSet<ITimeValue<T>>();

	@Override
	public ITimeValue<T> getValueAt(final Long time) {
		ITimeValue<T> tmp = null;
		for (ITimeValue<T> value : this.container) {
			if (value.getTime() <= time) {
				tmp = value;
			} else {
				break;
			}
		}
		return tmp;
	}

	@Override
	public void setValueAt(final Long time, final T targetValue) {
		ITimeValue<T> current = getValueAt(time);
		if (current != null && current.getTime().equals(time)) {
			current.setValue(targetValue);
		} else {
			this.container.add(new TimeValue<T>(time, targetValue));
		}
	}

	@Override
	public SortedSet<ITimeValue<T>> getFutureValues(final Long time) {
		TimeValue<T> picker = new TimeValue<T>(time, null);
		return new TreeSet<ITimeValue<T>>(this.container.tailSet(picker));
	}

	@Override
	public Collection<ITimeValue<T>> getPastValues(final Long time) {
		TimeValue<T> picker = new TimeValue<T>(time, null);
		return new TreeSet<ITimeValue<T>>(this.container.headSet(picker));
	}
	
	@Override
	public SortedSet<ITimeValue<T>> getValues() {
		return new TreeSet<ITimeValue<T>>(this.container);
	}

	@Override
	public void applyChange(final IValueChange<T> change) {

		SortedSet<ITimeValue<T>> futureValues = getFutureValues(change.getTime());
		for (ITimeValue<T> value : futureValues) {
			value.add(change.getValue());
		}

		ITimeValue<T> initialValue = getValueAt(change.getTime());
		if (initialValue == null) {
			ITimeValue<T> newValue = new TimeValue<T>(change.getTime(), change.getValue());
			this.container.add(newValue);
		} else if (initialValue.getTime().longValue() < change.getTime().longValue()) {
			ITimeValue<T> newValue = new TimeValue<T>(change.getTime(), initialValue.getValue());
			newValue.add(change.getValue()); 
			this.container.add(newValue);
		}
		compact();
	}

	@SuppressWarnings("unchecked")
	@Override
	public void compact() {

		if (this.container.size() < 2)
			return;

		Iterator<ITimeValue<T>> iterator = this.container.iterator();
		ITimeValue<T> predecessor = iterator.next();

		while (iterator.hasNext()) {
			ITimeValue<T> successor = iterator.next();
			if (successor.getValue().matches(predecessor.getValue())) {
				iterator.remove();
			} else {
				predecessor = successor;
			}
		}

	}

}
