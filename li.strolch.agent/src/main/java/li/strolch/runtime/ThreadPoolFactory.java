package li.strolch.runtime;

import java.util.concurrent.ThreadFactory;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * Simple {@link ThreadFactory} with which the thread pool name can be defined
 * 
 * @see java.util.concurrent.Executors.DefaultThreadFactory
 */
public class ThreadPoolFactory implements ThreadFactory {
	private final ThreadGroup group;
	private final AtomicInteger threadNumber = new AtomicInteger(1);
	private final String poolName;

	public ThreadPoolFactory(String poolName) {
		SecurityManager s = System.getSecurityManager();
		this.group = (s != null) ? s.getThreadGroup() : Thread.currentThread().getThreadGroup();
		this.poolName = poolName + "-";
	}

	@Override
	public Thread newThread(Runnable r) {
		Thread t = new Thread(this.group, r, this.poolName + this.threadNumber.getAndIncrement(), 0);
		if (t.isDaemon())
			t.setDaemon(false);
		if (t.getPriority() != Thread.NORM_PRIORITY)
			t.setPriority(Thread.NORM_PRIORITY);
		return t;
	}
}