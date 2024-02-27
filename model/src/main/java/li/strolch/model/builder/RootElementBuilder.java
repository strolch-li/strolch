package li.strolch.model.builder;

import li.strolch.model.StrolchRootElement;

import static li.strolch.model.builder.BuilderHelper.buildParamName;

public abstract class RootElementBuilder<T extends ParameterBagContainerBuilder<T>> extends PolicyContainerBuilder<T> {

	public RootElementBuilder(String id, String type) {
		super(id, buildParamName(id), type);
	}

	public RootElementBuilder(String id, String name, String type) {
		super(id, name, type);
	}

	protected void applyRootElement(StrolchRootElement element) {
		applyPolicyContainer(element);
	}

	public abstract StrolchRootElement build();
}
