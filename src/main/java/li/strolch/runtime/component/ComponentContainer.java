package li.strolch.runtime.component;

import java.text.MessageFormat;
import java.util.HashMap;
import java.util.Map;

import li.strolch.runtime.configuration.RuntimeConfiguration;

public class ComponentContainer {

	private Map<Class<?>, StrolchComponent> componentMap;

	public ComponentContainer() {
		this.componentMap = new HashMap<>();
	}

	@SuppressWarnings("unchecked")
	public <T extends StrolchComponent> T getComponent(Class<T> clazz) {
		T component = (T) this.componentMap.get(clazz);
		if (component == null) {
			String msg = "The component does not exist for class {0}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, clazz);
			throw new IllegalArgumentException(msg);
		}
		return component;
	}

	public void setup(RuntimeConfiguration configuration) {

	}

	public void start() {

	}

	public void stop() {

	}

	public void destroy() {

	}
}
