package li.strolch.model.builder.states;

import li.strolch.model.builder.ResourceBuilder;
import li.strolch.model.timedstate.IntegerTimedState;
import li.strolch.model.timevalue.impl.IntegerValue;

public class IntegerStateBuilder extends TimedStateBuilder<IntegerTimedState> {

	public IntegerStateBuilder(ResourceBuilder builder, String id, String name) {
		super(builder, id, name);
	}

	@Override
	public IntegerTimedState build() {
		IntegerTimedState state = new IntegerTimedState();
		applyTimedState(state);
		state.getTimeEvolution().setValueAt(0L, new IntegerValue(0));
		return state;
	}
}
