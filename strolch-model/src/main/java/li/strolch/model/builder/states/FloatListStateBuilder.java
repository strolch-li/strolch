package li.strolch.model.builder.states;

import li.strolch.model.builder.ResourceBuilder;
import li.strolch.model.timedstate.FloatListTimedState;
import li.strolch.model.timevalue.impl.FloatListValue;

public class FloatListStateBuilder extends TimedStateBuilder<FloatListTimedState> {

	public FloatListStateBuilder(ResourceBuilder builder, String id, String name) {
		super(builder, id, name);
	}

	@Override
	public FloatListTimedState build() {
		FloatListTimedState state = new FloatListTimedState();
		applyTimedState(state);
		state.getTimeEvolution().setValueAt(0L, new FloatListValue());
		return state;
	}
}
