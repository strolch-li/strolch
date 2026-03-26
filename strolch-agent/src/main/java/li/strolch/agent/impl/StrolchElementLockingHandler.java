package li.strolch.agent.impl;

import li.strolch.agent.api.StrolchAgent;
import li.strolch.utils.collections.TypedTuple;
import li.strolch.utils.concurrent.ElementLock;
import li.strolch.utils.concurrent.ElementLockingHandler;

import java.util.concurrent.TimeUnit;

public class StrolchElementLockingHandler<T> extends ElementLockingHandler<T> {

	private final StrolchAgent agent;

	public StrolchElementLockingHandler(StrolchAgent agent, TimeUnit tryLockTimeUnit, long tryLockTime) {
		super(agent.getScheduledExecutor("LockingHandler"), tryLockTimeUnit, tryLockTime);
		this.agent = agent;
	}

	@Override
	protected TypedTuple<ElementLock, Long> newLock(T element) {
		return new TypedTuple<>(new StrolchElementLock(element.toString(), true, this::recordRetriedLock), 0L);
	}

	private void recordRetriedLock() {
		this.agent.getAgentStatistics().recordRetriedLock();
	}
}
