package li.strolch.execution;

import static li.strolch.utils.helper.StringHelper.formatMillisecondsDuration;

import java.time.Duration;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.StrolchAgent;
import li.strolch.model.Locator;
import li.strolch.utils.time.PeriodDuration;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class SimpleDurationExecutionTimer implements DelayedExecutionTimer {

	private static final Logger logger = LoggerFactory.getLogger(SimpleDurationExecutionTimer.class);

	private final Map<Locator, ScheduledFuture<?>> simulationTasks;

	private final StrolchAgent agent;

	public SimpleDurationExecutionTimer(StrolchAgent agent) {
		this.agent = agent;
		this.simulationTasks = Collections.synchronizedMap(new HashMap<>());
	}

	@Override
	public void destroy() {
		this.simulationTasks.values().forEach(task -> task.cancel(false));
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
	public ScheduledFuture<?> delay(PeriodDuration duration, Runnable runnable) {
		return delay(duration.toMillis(), runnable);
	}

	@Override
	public ScheduledFuture<?> delay(Duration duration, Runnable runnable) {
		return delay(duration.toMillis(), runnable);
	}

	@Override
	public ScheduledFuture<?> delay(long duration, Runnable runnable) {
		return getExecutor().schedule(runnable, duration, TimeUnit.MILLISECONDS);
	}

	@Override
	public void execute(String realm, ComponentContainer container, Locator actionLocator, long duration) {
		synchronized (this.simulationTasks) {
			if (this.simulationTasks.containsKey(actionLocator)) {
				logger.warn("Ignoring duplicate timer for locator " + actionLocator);
			} else {
				SimulationTask task = new SimulationTask(realm, container, actionLocator);
				ScheduledFuture<?> future = getExecutor().schedule(task, duration, TimeUnit.MILLISECONDS);
				this.simulationTasks.put(actionLocator, future);
				logger.info("Scheduled a delay of " + formatMillisecondsDuration(duration) + " for " + actionLocator);
			}
		}
	}

	private ScheduledExecutorService getExecutor() {
		return this.agent.getScheduledExecutor("DurationExecution");
	}

	private void executed(String realm, ComponentContainer container, Locator locator) {

		this.simulationTasks.remove(locator);
		ExecutionHandler executionHandler = container.getComponent(ExecutionHandler.class);

		logger.info("Completing task " + locator);
		executionHandler.toExecuted(realm, locator);
	}

	private class SimulationTask implements Runnable {

		private final String realm;
		private final ComponentContainer container;
		private final Locator locator;

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
