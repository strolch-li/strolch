package li.strolch.model.builder;

import li.strolch.model.PolicyContainer;
import li.strolch.model.StrolchRootElement;

public abstract class RootElementBuilder<T extends ParameterBagContainerBuilder<T>> extends PolicyContainerBuilder<T> {

	public RootElementBuilder(String id, String name, String type) {
		super(id, name, type);
	}

	protected void applyRootElement(StrolchRootElement element) {
		applyPolicyContainer(element);
	}

	public abstract StrolchRootElement build();
}
