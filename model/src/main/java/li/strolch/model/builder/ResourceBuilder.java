package li.strolch.model.builder;

import java.util.ArrayList;
import java.util.List;

import li.strolch.model.Resource;
import li.strolch.model.builder.states.*;
import li.strolch.utils.dbc.DBC;

public class ResourceBuilder extends RootElementBuilder<ResourceBuilder> {

	private final StrolchElementBuilder builder;
	private final List<TimedStateBuilder<?>> timedStates;

	public ResourceBuilder(String id, String name, String type) {
		super(id, name, type);
		this.builder = null;
		this.timedStates = new ArrayList<>();
	}

	public ResourceBuilder(StrolchElementBuilder builder, String id, String name, String type) {
		super(id, name, type);
		this.builder = builder;
		this.timedStates = new ArrayList<>();
	}

	public BooleanStateBuilder booleanState(String id, String name) {
		BooleanStateBuilder builder = new BooleanStateBuilder(this, id, name);
		this.timedStates.add(builder);
		return builder;
	}

	public FloatStateBuilder floatState(String id, String name) {
		FloatStateBuilder builder = new FloatStateBuilder(this, id, name);
		this.timedStates.add(builder);
		return builder;
	}

	public IntegerStateBuilder integerState(String id, String name) {
		IntegerStateBuilder builder = new IntegerStateBuilder(this, id, name);
		this.timedStates.add(builder);
		return builder;
	}

	public LongStateBuilder longState(String id, String name) {
		LongStateBuilder builder = new LongStateBuilder(this, id, name);
		this.timedStates.add(builder);
		return builder;
	}

	public FloatListStateBuilder floatListState(String id, String name) {
		FloatListStateBuilder builder = new FloatListStateBuilder(this, id, name);
		this.timedStates.add(builder);
		return builder;
	}

	public IntegerListStateBuilder integerListState(String id, String name) {
		IntegerListStateBuilder builder = new IntegerListStateBuilder(this, id, name);
		this.timedStates.add(builder);
		return builder;
	}

	public StringSetStateBuilder stringSetState(String id, String name) {
		StringSetStateBuilder builder = new StringSetStateBuilder(this, id, name);
		this.timedStates.add(builder);
		return builder;
	}

	public StrolchElementBuilder endResource() {
		DBC.PRE.assertNotNull("Can not end, as not part of a builder context!", this.builder);
		return this.builder;
	}

	@Override
	public Resource build() {
		Resource resource = new Resource(getId(), getName(), getType());
		super.applyRootElement(resource);
		this.timedStates.forEach(b -> resource.addTimedState(b.build()));
		return resource;
	}
}
