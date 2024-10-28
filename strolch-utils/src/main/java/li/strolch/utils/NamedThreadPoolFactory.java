package li.strolch.utils;

import java.util.concurrent.ThreadFactory;

/**
 * Simple {@link ThreadFactory} which allocates as a pool and has a name for each pool
 */
public class NamedThreadPoolFactory implements ThreadFactory {
	private final ThreadFactory factory;

	public NamedThreadPoolFactory(String poolName) {
		this.factory = Thread.ofPlatform().name(poolName + "-", 0).factory();
	}

	@Override
	public Thread newThread(Runnable r) {
		Thread t = this.factory.newThread(r);
		if (t.getPriority() != Thread.NORM_PRIORITY)
			t.setPriority(Thread.NORM_PRIORITY);
		return t;
	}
}