package li.strolch.execution;

import java.util.Set;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.execution.command.ExecuteActivityCommand;
import li.strolch.execution.command.SetActionToErrorCommand;
import li.strolch.execution.command.SetActionToExecutedCommand;
import li.strolch.execution.command.SetActionToStoppedCommand;
import li.strolch.execution.command.SetActionToWarningCommand;
import li.strolch.execution.policy.ExecutionPolicy;
import li.strolch.model.Locator;
import li.strolch.model.State;
import li.strolch.model.activity.Action;
import li.strolch.model.activity.Activity;
import li.strolch.model.activity.IActivityElement;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Certificate;
import li.strolch.privilege.model.PrivilegeContext;
import li.strolch.runtime.ThreadPoolFactory;
import li.strolch.runtime.configuration.ComponentConfiguration;
import li.strolch.utils.collections.MapOfSets;
import li.strolch.utils.dbc.DBC;

/**
 * The event based execution handler waits for events in that the {@link ExecutionPolicy} implementations must call the
 * relevant methods when the work is complete. Afterwards the next {@link Action} in the procedure is executed
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class EventBasedExecutionHandler extends ExecutionHandler {

	private ExecutorService executorService;

	private MapOfSets<String, Locator> registeredActivities;

	private DelayedExecutionTimer delayedExecutionTimer;

	public EventBasedExecutionHandler(ComponentContainer container, String componentName) {
		super(container, componentName);
	}

	@Override
	public void initialize(ComponentConfiguration configuration) throws Exception {

		this.registeredActivities = new MapOfSets<>();

		super.initialize(configuration);
	}

	@Override
	public void start() throws Exception {

		this.executorService = Executors.newCachedThreadPool(new ThreadPoolFactory("ExecutionHandler"));
		this.delayedExecutionTimer = new SimpleDurationExecutionTimer();

		super.start();
	}

	@Override
	public void stop() throws Exception {

		if (this.executorService != null) {
			this.executorService.shutdown();
			while (!this.executorService.isTerminated()) {
				logger.info("Waiting for executor service to terminate...");
				Thread.sleep(50L);
			}
			this.executorService = null;
		}

		if (this.delayedExecutionTimer != null) {
			this.delayedExecutionTimer.destroy();
			this.delayedExecutionTimer = null;
		}

		super.stop();
	}

	@Override
	public void addForExecution(String realm, Locator activityLoc) {
		Locator rootElemLoc = activityLoc.trim(3);
		synchronized (this.registeredActivities) {
			this.registeredActivities.addElement(realm, rootElemLoc);
		}
		toExecution(realm, activityLoc);
	}

	@Override
	public void removeFromExecution(String realm, Locator activityLoc) {
		Locator rootElemLoc = activityLoc.trim(3);
		synchronized (this.registeredActivities) {
			this.registeredActivities.removeElement(realm, rootElemLoc);
		}
	}

	@Override
	public void toExecution(String realm, Locator locator) {
		this.executorService.execute(() -> {
			runAsAgent(ctx -> {
				toExecution(realm, locator, ctx);
			});
		});
	}

	@Override
	public void toExecuted(String realm, Locator locator) {
		this.executorService.execute(() -> {
			runAsAgent(ctx -> {
				toExecuted(realm, locator, ctx);
			});
		});
	}

	@Override
	public void toStopped(String realm, Locator locator) {
		this.executorService.execute(() -> {
			runAsAgent(ctx -> {
				toStopped(realm, locator, ctx);
			});
		});
	}

	@Override
	public void toError(String realm, Locator locator) {
		this.executorService.execute(() -> {
			runAsAgent(ctx -> {
				toError(realm, locator, ctx);
			});
		});
	}

	@Override
	public void toWarning(String realm, Locator locator) {
		this.executorService.execute(() -> {
			runAsAgent(ctx -> {
				toWarning(realm, locator, ctx);
			});
		});
	}

	private void toExecution(String realm, Locator activityLoc, PrivilegeContext ctx) {
		try (StrolchTransaction tx = openTx(realm, ctx.getCertificate(), ExecuteActivityCommand.class)) {
			Locator rootElemLoc = activityLoc.trim(3);
			tx.lock(rootElemLoc);

			IActivityElement elem = tx.findElement(rootElemLoc);
			if (elem == null) {
				logger.error("Element for locator " + activityLoc + " does not exist!");
				synchronized (this.registeredActivities) {
					this.registeredActivities.removeElement(realm, rootElemLoc);
				}
				return;
			}

			DBC.INTERIM.assertEquals("toExecution only for Activity!", Activity.class, elem.getClass());

			ExecuteActivityCommand command = new ExecuteActivityCommand(getContainer(), tx);
			command.setActivity((Activity) elem);
			tx.addCommand(command);

			tx.commitOnClose();
		}
	}

	private void toExecuted(String realm, Locator actionLoc, PrivilegeContext ctx) {

		Locator activityLoc = actionLoc.trim(3);

		try (StrolchTransaction tx = openTx(realm, ctx.getCertificate(), SetActionToExecutedCommand.class)) {
			tx.lock(activityLoc);

			IActivityElement elem = tx.findElement(actionLoc);
			DBC.INTERIM.assertEquals("toExecuted only for Action!", Action.class, elem.getClass());

			SetActionToExecutedCommand command = new SetActionToExecutedCommand(getContainer(), tx);
			command.setAction((Action) elem);
			tx.addCommand(command);

			tx.flush();

			if (elem.getRootElement().getState() == State.EXECUTED) {
				synchronized (this.registeredActivities) {
					this.registeredActivities.removeElement(realm, activityLoc);
				}
			}

			tx.commitOnClose();
		}

		// execute any next Action
		toExecution(realm, activityLoc, ctx);

		// now trigger a further execution of any other activities needed execution in this realm
		triggerExecution(realm);
	}

	/**
	 * Triggers a to execution for all registered activities in the given realm
	 * 
	 * @param realm
	 */
	private void triggerExecution(String realm) {
		synchronized (this.registeredActivities) {
			Set<Locator> locators = this.registeredActivities.getSet(realm);
			if (locators != null) {
				for (Locator locator : locators) {
					// execute async
					toExecution(realm, locator);
				}
			}
		}
	}

	private void toWarning(String realm, Locator actionLoc, PrivilegeContext ctx) {
		try (StrolchTransaction tx = openTx(realm, ctx.getCertificate(), SetActionToExecutedCommand.class)) {
			Locator rootElemLoc = actionLoc.trim(3);
			tx.lock(rootElemLoc);

			IActivityElement elem = tx.findElement(actionLoc);
			DBC.INTERIM.assertEquals("toWarning only for Action!", Action.class, elem.getClass());

			SetActionToWarningCommand command = new SetActionToWarningCommand(getContainer(), tx);
			command.setAction((Action) elem);
			tx.addCommand(command);

			tx.commitOnClose();
		}
	}

	private void toError(String realm, Locator actionLoc, PrivilegeContext ctx) {
		try (StrolchTransaction tx = openTx(realm, ctx.getCertificate(), SetActionToExecutedCommand.class)) {
			Locator rootElemLoc = actionLoc.trim(3);
			tx.lock(rootElemLoc);

			IActivityElement elem = tx.findElement(actionLoc);
			DBC.INTERIM.assertEquals("toError only for Action!", Action.class, elem.getClass());

			SetActionToErrorCommand command = new SetActionToErrorCommand(getContainer(), tx);
			command.setAction((Action) elem);
			tx.addCommand(command);

			tx.commitOnClose();
		}
	}

	private void toStopped(String realm, Locator actionLoc, PrivilegeContext ctx) {
		try (StrolchTransaction tx = openTx(realm, ctx.getCertificate(), SetActionToStoppedCommand.class)) {
			Locator rootElemLoc = actionLoc.trim(3);
			tx.lock(rootElemLoc);

			IActivityElement elem = tx.findElement(actionLoc);
			DBC.INTERIM.assertEquals("toStopped only for Action!", Action.class, elem.getClass());

			SetActionToStoppedCommand command = new SetActionToStoppedCommand(getContainer(), tx);
			command.setAction((Action) elem);
			tx.addCommand(command);

			tx.commitOnClose();
		}

		// now trigger a further execution of any other activities needed execution in this realm
		triggerExecution(realm);
	}

	protected StrolchTransaction openTx(String realm, Certificate cert, Class<?> clazz) {
		return getContainer().getRealm(realm).openTx(cert, clazz);
	}

	@Override
	public DelayedExecutionTimer getDelayedExecutionTimer() {
		return this.delayedExecutionTimer;
	}
}
