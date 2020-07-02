package li.strolch.model.builder.states;

import li.strolch.model.builder.ResourceBuilder;
import li.strolch.model.timedstate.FloatTimedState;

public class FloatStateBuilder extends TimedStateBuilder<FloatTimedState> {

	public FloatStateBuilder(ResourceBuilder builder, String id, String name) {
		super(builder, id, name);
	}

	@Override
	public FloatTimedState build() {
		return build(new FloatTimedState());
	}
}
