package li.strolch.model.builder.states;

import li.strolch.model.builder.ResourceBuilder;
import li.strolch.model.timedstate.IntegerListTimedState;
import li.strolch.model.timevalue.impl.IntegerListValue;

public class IntegerListStateBuilder extends TimedStateBuilder<IntegerListTimedState> {

	public IntegerListStateBuilder(ResourceBuilder builder, String id, String name) {
		super(builder, id, name);
	}

	@Override
	public IntegerListTimedState build() {
		IntegerListTimedState state = new IntegerListTimedState();
		applyTimedState(state);
		state.getTimeEvolution().setValueAt(0L, new IntegerListValue());
		return state;
	}
}
