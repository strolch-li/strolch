package li.strolch.model.builder.states;

import li.strolch.model.builder.ResourceBuilder;
import li.strolch.model.timedstate.LongTimedState;
import li.strolch.model.timevalue.impl.LongValue;

public class LongStateBuilder extends TimedStateBuilder<LongTimedState> {

	public LongStateBuilder(ResourceBuilder builder, String id, String name) {
		super(builder, id, name);
	}

	@Override
	public LongTimedState build() {
		LongTimedState state = new LongTimedState();
		build(state);
		state.getTimeEvolution().setValueAt(0L, new LongValue(0L));
		return state;
	}
}
