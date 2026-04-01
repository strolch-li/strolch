package li.strolch.agent.impl;

import li.strolch.utils.concurrent.ElementLock;

public class StrolchElementLock extends ElementLock {

	private final Runnable retryHandler;

	public StrolchElementLock(String name, boolean fair, Runnable retryHandler) {
		super(name, fair);
		this.retryHandler = retryHandler;
	}

	@Override
	protected void handleRetry() {
		this.retryHandler.run();
	}
}
