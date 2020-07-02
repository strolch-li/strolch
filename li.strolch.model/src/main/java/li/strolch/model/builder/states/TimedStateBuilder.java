package li.strolch.model.builder.states;

import static li.strolch.model.StrolchModelConstants.INTERPRETATION_NONE;
import static li.strolch.model.StrolchModelConstants.UOM_NONE;

import li.strolch.model.builder.ResourceBuilder;
import li.strolch.model.timedstate.StrolchTimedState;

public abstract class TimedStateBuilder<T extends StrolchTimedState<?>> {

	private final ResourceBuilder builder;
	protected final String id;
	protected final String name;

	protected boolean hidden = false;
	protected int index;
	protected String interpretation = INTERPRETATION_NONE;
	protected String uom = UOM_NONE;

	public TimedStateBuilder(ResourceBuilder builder, String id, String name) {
		this.builder = builder;
		this.id = id;
		this.name = name;
	}

	public ResourceBuilder end() {
		return this.builder;
	}

	public abstract T build();

	protected T build(T state) {
		state.setId(this.id);
		state.setName(this.name);
		state.setHidden(this.hidden);
		state.setIndex(this.index);
		state.setInterpretation(this.interpretation);
		state.setUom(this.uom);
		return state;
	}
}
