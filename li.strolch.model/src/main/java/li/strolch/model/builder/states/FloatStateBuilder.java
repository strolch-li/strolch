package li.strolch.model.builder.states;

import li.strolch.model.builder.ResourceBuilder;
import li.strolch.model.timedstate.FloatTimedState;
import li.strolch.model.timevalue.impl.FloatValue;

public class FloatStateBuilder extends TimedStateBuilder<FloatTimedState> {

	public FloatStateBuilder(ResourceBuilder builder, String id, String name) {
		super(builder, id, name);
	}

	@Override
	public FloatTimedState build() {
		FloatTimedState state = new FloatTimedState();
		build(state);
		state.getTimeEvolution().setValueAt(0L, new FloatValue(0.0D));
		return state;
	}
}
