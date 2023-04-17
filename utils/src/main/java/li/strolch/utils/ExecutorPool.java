package li.strolch.utils;

import static java.util.concurrent.Executors.*;

import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import li.strolch.utils.dbc.DBC;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ExecutorPool {

	private static final Logger logger = LoggerFactory.getLogger(ExecutorPool.class);

	private final Map<String, ExecutorService> executors;
	private final Map<String, ScheduledExecutorService> scheduledExecutors;

	public ExecutorPool() {
		this.executors = new ConcurrentHashMap<>();
		this.scheduledExecutors = new ConcurrentHashMap<>();
	}

	public ExecutorService getExecutor(String poolName) {
		DBC.PRE.assertNotEmpty("poolName must be set!", poolName);
		return this.executors.computeIfAbsent(poolName, p -> newCachedThreadPool(new NamedThreadPoolFactory(p)));
	}

	public ExecutorService getSingleThreadExecutor(String poolName) {
		DBC.PRE.assertNotEmpty("poolName must be set!", poolName);
		return this.executors.computeIfAbsent(poolName, p -> newSingleThreadExecutor(new NamedThreadPoolFactory(p)));
	}

	public ScheduledExecutorService getScheduledExecutor(String poolName) {
		DBC.PRE.assertNotEmpty("poolName must be set!", poolName);
		return this.scheduledExecutors.computeIfAbsent(poolName,
				p -> newScheduledThreadPool(4, new NamedThreadPoolFactory(p)));
	}

	public void destroy() {
		this.executors.forEach(this::shutdownExecutor);
		this.scheduledExecutors.forEach(this::shutdownExecutor);
	}

	private void shutdownExecutor(String name, ExecutorService executor) {
		logger.info("Shutting down executor pool " + name);
		try {
			List<Runnable> tasks = executor.shutdownNow();
			if (!tasks.isEmpty()) {
				logger.warn("The following " + tasks.size() + " tasks were never started for executor " + name + " :");
				for (Runnable runnable : tasks) {
					logger.warn("  " + runnable);
				}
			}

			if (!executor.awaitTermination(5, TimeUnit.SECONDS))
				logger.error("Executor " + name + " did not stop after " + 5 + "s!");
		} catch (InterruptedException e) {
			logger.error("Was interrupted while shutting down tasks");
		}
	}
}
