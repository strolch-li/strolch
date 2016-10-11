package li.strolch.execution;

import java.util.HashMap;
import java.util.Map;
import java.util.Timer;
import java.util.TimerTask;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.model.Locator;

public class SimpleDurationExecutionTimer implements DelayedExecutionTimer {

	private static final Logger logger = LoggerFactory.getLogger(SimpleDurationExecutionTimer.class);

	private Timer timer;

	private Map<Locator, SimulationTask> simulationPolicies;

	public SimpleDurationExecutionTimer() {
		this.simulationPolicies = new HashMap<>();
	}

	@Override
	public void destroy() {
		if (this.timer != null) {
			this.timer.cancel();
		}
	}

	@Override
	public void cancel(Locator locator) {
		SimulationTask task = this.simulationPolicies.remove(locator);
		if (task != null) {
			task.cancel();
		}
	}

	@Override
	public void execute(String realm, ComponentContainer container, Locator locator, long duration) {
		if (this.timer == null)
			this.timer = new Timer("SimulationExecution", true);

		SimulationTask task = new SimulationTask(realm, container, locator);
		this.simulationPolicies.put(locator, task);
		this.timer.schedule(task, duration);

		logger.info("Registered execution timer for " + locator);
	}

	private void executed(String realm, ComponentContainer container, Locator locator) {

		logger.info("Completing execution for " + locator);

		ExecutionHandler executionHandler = container.getComponent(ExecutionHandler.class);
		executionHandler.toExecuted(realm, locator);
	}

	private class SimulationTask extends TimerTask {

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
