package li.strolch.model;

public abstract class AbstractStrolchRootElement extends GroupedParameterizedElement implements StrolchRootElement {

	public AbstractStrolchRootElement() {
		super();
	}

	public AbstractStrolchRootElement(String id, String name, String type) {
		super(id, name, type);
	}
}
