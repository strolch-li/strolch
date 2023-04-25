package li.strolch.model.builder.states;

import static java.util.Collections.emptySet;

import li.strolch.model.builder.ResourceBuilder;
import li.strolch.model.timedstate.StringSetTimedState;
import li.strolch.model.timevalue.impl.StringSetValue;

public class StringSetStateBuilder extends TimedStateBuilder<StringSetTimedState> {

	public StringSetStateBuilder(ResourceBuilder builder, String id, String name) {
		super(builder, id, name);
	}

	@Override
	public StringSetTimedState build() {
		StringSetTimedState state = new StringSetTimedState();
		applyTimedState(state);
		state.getTimeEvolution().setValueAt(0L, new StringSetValue(emptySet()));
		return state;
	}
}
