package li.strolch.model.timedstate;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import li.strolch.model.timevalue.ITimeValue;
import li.strolch.model.timevalue.ITimeVariable;
import li.strolch.model.timevalue.IValue;
import li.strolch.model.timevalue.IValueChange;
import li.strolch.model.timevalue.impl.TimeVariable;

/**
 * @author Martin Smock <smock.martin@gmail.com>
 */
@SuppressWarnings("rawtypes")
public class TimedState<T extends IValue> implements ITimedState<T>, Serializable {

	private static final long serialVersionUID = 1L;
	
	private ITimeVariable<T> timeVariable = new TimeVariable<T>();

	@Override
	@SuppressWarnings("unchecked")
	public ITimeValue<T> getNextMatch(final Long time, final T value) {
		Collection<ITimeValue<T>> futureValues = timeVariable.getFutureValues(time);
		for (ITimeValue<T> iTimeValue : futureValues) {
			if (iTimeValue.getValue().matches(value)) {
				return iTimeValue;
			}
		}
		return null;
	}

	@Override
	@SuppressWarnings("unchecked")
	public ITimeValue<T> getPreviousMatch(final Long time, final T value) {
		Collection<ITimeValue<T>> pastValues = timeVariable.getPastValues(time);
		List<ITimeValue<T>> asList = new ArrayList<ITimeValue<T>>(pastValues);
		Collections.reverse(asList);
		for (ITimeValue<T> iTimeValue : asList) {
			if (iTimeValue.getValue().matches(value)) {
				return iTimeValue;
			}
		}
		return null;
	}

	@Override
	public void applyChange(final IValueChange<T> change) {
		timeVariable.applyChange(change);
	}

	@Override
	public ITimeValue<T> getStateAt(final Long time) {
		return timeVariable.getValueAt(time);
	}

	@Override
	public ITimeVariable<T> getTimeEvolution() {
		return timeVariable;
	}

}
