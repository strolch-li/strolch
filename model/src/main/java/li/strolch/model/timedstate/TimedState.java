/*
 * Copyright 2013 Martin Smock <smock.martin@gmail.com>
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
package li.strolch.model.timedstate;

import java.io.Serializable;
import java.util.NavigableSet;

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

	private ITimeVariable<T> timeVariable = new TimeVariable<>();

	@Override
	@SuppressWarnings("unchecked")
	public ITimeValue<T> getNextMatch(final Long time, final T value) {
		return this.timeVariable.getFutureValues(time).stream() //
				.filter(v -> v.getValue().matches(value)) //
				.findFirst().orElse(null);
	}

	@Override
	@SuppressWarnings("unchecked")
	public ITimeValue<T> getPreviousMatch(final Long time, final T value) {
		NavigableSet<ITimeValue<T>> pastValues = this.timeVariable.getPastValues(time);
		return pastValues.descendingSet().stream() //
				.filter(v -> v.getValue().matches(value)) //
				.findFirst().orElse(null);
	}

	@Override
	public <U extends IValueChange<T>> void applyChange(U change, boolean compact) {
		this.timeVariable.applyChange(change, compact);
	}

	@Override
	public ITimeValue<T> getStateAt(final Long time) {
		return this.timeVariable.getValueAt(time);
	}

	@Override
	public ITimeVariable<T> getTimeEvolution() {
		return this.timeVariable;
	}

	@Override
	public ITimedState<T> getCopy() {
		TimedState<T> copy = new TimedState<>();
		copy.timeVariable = this.timeVariable.getCopy();
		return copy;
	}

	@Override
	public boolean isReadonly() {
		return this.timeVariable.isReadonly();
	}

	@Override
	public void setReadonly() {
		this.timeVariable.setReadonly();
	}
}
