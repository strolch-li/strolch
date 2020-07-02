package li.strolch.model.builder.states;

import li.strolch.model.builder.ResourceBuilder;
import li.strolch.model.timedstate.IntegerTimedState;

public class IntegerStateBuilder extends TimedStateBuilder<IntegerTimedState> {

	public IntegerStateBuilder(ResourceBuilder builder, String id, String name) {
		super(builder, id, name);
	}

	@Override
	public IntegerTimedState build() {
		return build(new IntegerTimedState());
	}
}
