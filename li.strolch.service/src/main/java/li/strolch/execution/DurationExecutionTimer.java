package li.strolch.execution;

import java.util.HashMap;
import java.util.Map;
import java.util.Timer;
import java.util.TimerTask;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.execution.service.SetActionToExecutedService;
import li.strolch.model.Locator;
import li.strolch.runtime.StrolchConstants;
import li.strolch.service.LocatorArgument;
import li.strolch.service.api.ServiceHandler;
import li.strolch.service.api.ServiceResult;

public class DurationExecutionTimer {

	private static final Logger logger = LoggerFactory.getLogger(DurationExecutionTimer.class);

	private static final DurationExecutionTimer instance;

	static {
		instance = new DurationExecutionTimer();
	}

	public static DurationExecutionTimer getInstance() {
		return instance;
	}

	private Timer timer;

	private Map<Locator, SimulationTask> simulationPolicies;

	public DurationExecutionTimer() {
		this.simulationPolicies = new HashMap<>();
	}

	public void stop(Locator locator) {
		SimulationTask task = this.simulationPolicies.remove(locator);
		if (task != null) {
			task.cancel();
		}
	}

	public void execute(String realm, ComponentContainer container, Locator locator, long duration) {
		if (this.timer == null)
			this.timer = new Timer("SimulationExecution", true);

		SimulationTask task = new SimulationTask(realm, container, locator);
		this.simulationPolicies.put(locator, task);
		this.timer.schedule(task, duration);

		logger.info("Registered execution timer for " + locator);
	}

	private void executed(Locator locator, String realm, ComponentContainer container) {

		logger.info("Completing execution for " + locator);

		SetActionToExecutedService svc = new SetActionToExecutedService();
		LocatorArgument arg = new LocatorArgument();
		arg.realm = realm;
		arg.locator = locator;

		container.getPrivilegeHandler().runAsAgent(ctx -> {
			ServiceHandler svcHandler = container.getComponent(ServiceHandler.class);
			ServiceResult result = svcHandler.doService(ctx.getCertificate(), svc, arg);
		});
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
			executed(locator, realm, container);
		}
	}
}
