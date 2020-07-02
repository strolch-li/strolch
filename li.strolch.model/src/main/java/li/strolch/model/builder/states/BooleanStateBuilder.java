package li.strolch.model.builder.states;

import li.strolch.model.builder.ResourceBuilder;
import li.strolch.model.timedstate.BooleanTimedState;

public class BooleanStateBuilder extends TimedStateBuilder<BooleanTimedState> {

	public BooleanStateBuilder(ResourceBuilder builder, String id, String name) {
		super(builder, id, name);
	}

	@Override
	public BooleanTimedState build() {
		return build(new BooleanTimedState());
	}
}
