package li.strolch.model.builder.params;

import static li.strolch.model.StrolchModelConstants.INTERPRETATION_NONE;
import static li.strolch.model.StrolchModelConstants.UOM_NONE;

import li.strolch.model.builder.BagBuilder;
import li.strolch.model.builder.ParameterBagContainerBuilder;
import li.strolch.model.parameter.Parameter;

public abstract class ParameterBuilder<S extends Parameter<?>, T extends ParameterBagContainerBuilder<T>> {

	private final BagBuilder<T> builder;

	protected final String id;
	protected final String name;

	protected boolean hidden = false;
	protected int index;
	protected String interpretation = INTERPRETATION_NONE;
	protected String uom = UOM_NONE;

	public ParameterBuilder(BagBuilder<T> builder, String id, String name) {
		this.builder = builder;
		this.id = id;
		this.name = name;
	}

	public ParameterBuilder<S, T> hidden(boolean hidden) {
		this.hidden = hidden;
		return this;
	}

	public ParameterBuilder<S, T> index(int index) {
		this.index = index;
		return this;
	}

	public ParameterBuilder<S, T> interpretation(String interpretation) {
		this.interpretation = interpretation;
		return this;
	}

	public ParameterBuilder<S, T> uom(String uom) {
		this.uom = uom;
		return this;
	}

	public BagBuilder<T> end() {
		return this.builder;
	}

	protected S build(S parameter) {
		parameter.setId(this.id);
		parameter.setName(this.name);
		parameter.setHidden(this.hidden);
		parameter.setIndex(this.index);
		parameter.setInterpretation(this.interpretation);
		parameter.setUom(this.uom);
		return parameter;
	}

	public abstract S build();
}
