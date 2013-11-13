package li.strolch.runtime.component;

import java.text.MessageFormat;

import li.strolch.runtime.configuration.ComponentConfiguration;

public class StrolchComponent {

	private final ComponentContainer container;
	private final String componentName;
	private ComponentState state;

	public StrolchComponent(ComponentContainer container, String componentName) {
		this.container = container;
		this.componentName = componentName;
		this.state = ComponentState.UNDEFINED;
	}

	/**
	 * @return the componentName
	 */
	public String getName() {
		return this.componentName;
	}

	public ComponentState getState() {
		return this.state;
	}

	protected ComponentContainer getContainer() {
		return this.container;
	}

	protected void assertStarted() {
		if (getState() != ComponentState.STARTED) {
			String msg = "Component {0} is not yet started!"; //$NON-NLS-1$
			throw new IllegalStateException(MessageFormat.format(msg, this.componentName));
		}
	}

	protected void assertContainerStarted() {
		if (getContainer().getState() != ComponentState.STARTED) {
			String msg = "Container is not yet started!"; //$NON-NLS-1$
			throw new IllegalStateException(msg);
		}
	}

	public void initialize(ComponentConfiguration configuration) {
		this.state = this.state.validateStateChange(ComponentState.INITIALIZED);
	}

	public void start() {
		this.state = this.state.validateStateChange(ComponentState.STARTED);
	}

	public void stop() {
		this.state = this.state.validateStateChange(ComponentState.STOPPED);
	}

	public void destroy() {
		this.state = this.state.validateStateChange(ComponentState.DESTROYED);
	}
}
