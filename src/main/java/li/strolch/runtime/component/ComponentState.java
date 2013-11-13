package li.strolch.runtime.component;

import java.text.MessageFormat;

public enum ComponentState {

	UNDEFINED, INITIALIZED, STARTED, STOPPED, DESTROYED;

	public ComponentState validateStateChange(ComponentState newState) {

		if (this == newState)
			return this;

		switch (this) {
		case UNDEFINED:
			if (newState != ComponentState.INITIALIZED)
				throw getIllegalStateEx(newState);
			break;
		case INITIALIZED:
			if (newState != ComponentState.STARTED)
				throw getIllegalStateEx(newState);
			break;
		case STARTED:
			if (newState != ComponentState.STOPPED)
				throw getIllegalStateEx(newState);
			break;
		case STOPPED:
			if (newState != ComponentState.STARTED && newState != ComponentState.DESTROYED)
				throw getIllegalStateEx(newState);
			break;
		case DESTROYED:
			throw getIllegalStateEx(newState);
		default:
			throw getIllegalStateEx(newState);
		}

		return newState;
	}

	private IllegalStateException getIllegalStateEx(ComponentState newState) {
		String msg = "Moving from state {0} to state {1} is not allowed!"; //$NON-NLS-1$
		msg = MessageFormat.format(msg, this, newState);
		throw new IllegalStateException(msg);
	}
}
