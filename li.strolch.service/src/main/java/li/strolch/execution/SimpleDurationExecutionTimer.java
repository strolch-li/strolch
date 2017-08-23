package li.strolch.execution;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.model.Locator;
import li.strolch.runtime.ThreadPoolFactory;

public class SimpleDurationExecutionTimer implements DelayedExecutionTimer {

	private static final Logger logger = LoggerFactory.getLogger(SimpleDurationExecutionTimer.class);

	private Map<Locator, ScheduledFuture<?>> simulationTasks;

	private ScheduledExecutorService scheduledExecutorService;

	public SimpleDurationExecutionTimer() {
		this.scheduledExecutorService = Executors.newScheduledThreadPool(0, new ThreadPoolFactory("DurationExecution"));
		this.simulationTasks = new HashMap<>();
	}

	@Override
	public void destroy() {

		this.simulationTasks.values().forEach(task -> task.cancel(false));

		if (this.scheduledExecutorService != null) {
			this.scheduledExecutorService.shutdown();
			while (!this.scheduledExecutorService.isTerminated()) {
				logger.info("Waiting for executor service to terminate...");
				try {
					Thread.sleep(50L);
				} catch (InterruptedException e) {
					logger.warn("Interrupted!");
				}
			}
			this.scheduledExecutorService = null;
		}
	}

	@Override
	public void cancel(Locator locator) {
		ScheduledFuture<?> future = this.simulationTasks.remove(locator);
		if (future != null) {
			if (!future.cancel(false)) {
				logger.warn("Failed to cancel task " + locator);
			}
		}
	}

	@Override
	public void execute(String realm, ComponentContainer container, Locator actionLocator, long duration) {
		SimulationTask task = new SimulationTask(realm, container, actionLocator);
		ScheduledFuture<?> future = this.scheduledExecutorService.schedule(task, duration, TimeUnit.MILLISECONDS);
		this.simulationTasks.put(actionLocator, future);

		logger.info("Registered execution timer for " + actionLocator);
	}

	private void executed(String realm, ComponentContainer container, Locator locator) {

		logger.info("Completing execution for " + locator);

		ExecutionHandler executionHandler = container.getComponent(ExecutionHandler.class);
		executionHandler.toExecuted(realm, locator);
	}

	private class SimulationTask implements Runnable {

		private String realm;
		private ComponentContainer container;
		private Locator locator;

		public SimulationTask(String realm, ComponentContainer container, Locator locator) {
			this.realm = realm;
			this.container = container;
			this.locator = locator;
		}

		@Override
		public void run() {
			executed(realm, container, locator);
		}
	}
}
