package li.strolch.model.builder.states;

import li.strolch.model.builder.ResourceBuilder;
import li.strolch.model.timedstate.StringSetTimedState;

public class StringSetStateBuilder extends TimedStateBuilder<StringSetTimedState> {

	public StringSetStateBuilder(ResourceBuilder builder, String id, String name) {
		super(builder, id, name);
	}

	@Override
	public StringSetTimedState build() {
		return build(new StringSetTimedState());
	}
}
