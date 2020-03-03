package li.strolch.utils;

import static java.util.concurrent.Executors.*;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import li.strolch.utils.dbc.DBC;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ExecutorPool {

	private static final Logger logger = LoggerFactory.getLogger(ExecutorPool.class);

	private Map<String, ExecutorService> executors;
	private Map<String, ScheduledExecutorService> scheduledExecutors;

	public ExecutorPool() {
		this.executors = Collections.synchronizedMap(new HashMap<>());
		this.scheduledExecutors = Collections.synchronizedMap(new HashMap<>());
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
		return this.scheduledExecutors
				.computeIfAbsent(poolName, p -> newScheduledThreadPool(4, new NamedThreadPoolFactory(p)));
	}

	public void destroy() {

		for (String poolName : this.executors.keySet()) {
			logger.info("Shutting down executor pool " + poolName);
			ExecutorService executor = this.executors.get(poolName);

			shutdownExecutor(executor);
		}
		for (String poolName : this.scheduledExecutors.keySet()) {
			logger.info("Shutting down scheduled executor pool " + poolName);
			ExecutorService executor = this.scheduledExecutors.get(poolName);

			shutdownExecutor(executor);
		}
	}

	private void shutdownExecutor(ExecutorService executor) {
		try {
			executor.shutdown();
			executor.awaitTermination(5, TimeUnit.SECONDS);
		} catch (InterruptedException e) {
			logger.error("Was interrupted while shutting down tasks");
		} finally {
			if (!executor.isTerminated()) {
				logger.error("Tasks not stopped after " + 5 + "s. Shutting down now.");
				executor.shutdownNow();
			}
		}
	}
}
