package li.strolch.runtime.component;

import java.text.MessageFormat;

import li.strolch.runtime.configuration.ComponentConfiguration;

public class StrolchComponent {

	private final String componentName;
	private ComponentState state;

	public StrolchComponent(String componentName) {
		this.componentName = componentName;
		this.state = ComponentState.UNDEFINED;
	}

	/**
	 * @return the componentName
	 */
	public String getComponentName() {
		return this.componentName;
	}

	public ComponentState getState() {
		return this.state;
	}

	private IllegalStateException getIllegalStateEx(ComponentState currentState, ComponentState newState) {
		String msg = "Moving from state {0} to state {1} is not allowed!"; //$NON-NLS-1$
		msg = MessageFormat.format(msg, currentState, newState);
		throw new IllegalStateException(msg);
	}

	protected void changeState(ComponentState newState) {
		switch (this.state) {
		case UNDEFINED:
			if (newState != ComponentState.INITALIZED)
				throw getIllegalStateEx(this.state, newState);
			break;
		case INITALIZED:
			if (newState != ComponentState.STARTED)
				throw getIllegalStateEx(this.state, newState);
			break;
		case STARTED:
			if (newState != ComponentState.STOPPED)
				throw getIllegalStateEx(this.state, newState);
			break;
		case STOPPED:
			if (newState != ComponentState.STARTED && newState != ComponentState.DESTROYED)
				throw getIllegalStateEx(this.state, newState);
			break;
		case DESTROYED:
			throw getIllegalStateEx(this.state, newState);
		default:
			throw getIllegalStateEx(this.state, newState);
		}

		this.state = newState;
	}

	protected void assertStarted() {
		if (getState() != ComponentState.STARTED) {
			String msg = "Component {0} is not yet started!"; //$NON-NLS-1$
			throw new IllegalStateException(MessageFormat.format(msg, this.componentName));
		}
	}

	/**
	 * @param configuration
	 */
	public void initialize(ComponentConfiguration configuration) {
		changeState(ComponentState.INITALIZED);
	}

	public void start() {
		changeState(ComponentState.STARTED);
	}

	public void stop() {
		changeState(ComponentState.STOPPED);
	}

	public void destroy() {
		changeState(ComponentState.DESTROYED);
	}
}
