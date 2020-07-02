package li.strolch.model.builder.states;

import li.strolch.model.builder.ResourceBuilder;
import li.strolch.model.timedstate.FloatListTimedState;

public class FloatListStateBuilder extends TimedStateBuilder<FloatListTimedState> {

	public FloatListStateBuilder(ResourceBuilder builder, String id, String name) {
		super(builder, id, name);
	}

	@Override
	public FloatListTimedState build() {
		return build(new FloatListTimedState());
	}
}
