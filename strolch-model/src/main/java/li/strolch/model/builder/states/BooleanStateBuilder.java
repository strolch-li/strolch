package li.strolch.model.builder.states;

import li.strolch.model.builder.ResourceBuilder;
import li.strolch.model.timedstate.BooleanTimedState;
import li.strolch.model.timevalue.impl.BooleanValue;

public class BooleanStateBuilder extends TimedStateBuilder<BooleanTimedState> {

	public BooleanStateBuilder(ResourceBuilder builder, String id, String name) {
		super(builder, id, name);
	}

	@Override
	public BooleanTimedState build() {
		BooleanTimedState state = new BooleanTimedState();
		applyTimedState(state);
		state.getTimeEvolution().setValueAt(0L, new BooleanValue(false));
		return state;
	}
}
