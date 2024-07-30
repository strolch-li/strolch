package li.strolch.execution;

public enum ExecutionHandlerState {
	Running,
	HaltNew,
	Paused;

	public boolean isRunning() {
		return this == Running;
	}

	public boolean isPaused() {
		return this == Paused;
	}

	public boolean isHaltNew() {
		return this == HaltNew;
	}
}
