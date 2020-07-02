package li.strolch.model.builder.states;

import li.strolch.model.builder.ResourceBuilder;
import li.strolch.model.timedstate.LongTimedState;

public class LongStateBuilder extends TimedStateBuilder<LongTimedState> {

	public LongStateBuilder(ResourceBuilder builder, String id, String name) {
		super(builder, id, name);
	}

	@Override
	public LongTimedState build() {
		return build(new LongTimedState());
	}
}
