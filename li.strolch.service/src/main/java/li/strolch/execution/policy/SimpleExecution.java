package li.strolch.execution.policy;

import java.util.concurrent.ScheduledFuture;
import java.util.function.BiConsumer;
import java.util.function.Consumer;
import java.util.function.Supplier;

import li.strolch.exception.StrolchException;
import li.strolch.execution.ExecutionHandler;
import li.strolch.handler.operationslog.OperationsLog;
import li.strolch.model.State;
import li.strolch.model.activity.Action;
import li.strolch.model.log.LogMessage;
import li.strolch.model.parameter.DurationParameter;
import li.strolch.model.timevalue.impl.FloatValue;
import li.strolch.model.timevalue.impl.ValueChange;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.PrivilegeContext;
import li.strolch.utils.time.PeriodDuration;

/**
 * <p>
 * Simple Execution Policy which sets the state of the action depending on the method called.
 * </p>
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class SimpleExecution extends ExecutionPolicy {

	private ScheduledFuture<?> warningTask;

	public SimpleExecution(StrolchTransaction tx) {
		super(tx);
	}

	protected void startWarningTask(Action action, Supplier<LogMessage> handler) {
		DurationParameter durationP = findActionDuration(action);
		startWarningTask(durationP.getValue(), action, handler);
	}

	protected void startWarningTask(PeriodDuration duration, Action action, Supplier<LogMessage> handler) {
		if (this.warningTask != null) {
			logger.warn("There is already a warning task registered, for action " + action.getLocator()
					+ ". Cancelling and creating a new task...");
			this.warningTask.cancel(true);
			this.warningTask = null;
		}

		this.warningTask = getDelayedExecutionTimer().delay(duration, () -> handleToWarning(handler));
		logger.info("Registered warning task for action " + this.actionLoc);
	}

	private void handleToWarning(Supplier<LogMessage> handler) {
		try {
			LogMessage logMessage = handler.get();
			logger.warn("Action " + this.actionLoc + " is in warning with message: " + logMessage.getMessage());
			toWarning(logMessage);
		} catch (Exception e) {
			logger.error("Failed to perform warning task for action " + this.actionLoc, e);
		}
	}

	protected void cancelWarningTask() {
		if (this.warningTask != null) {
			this.warningTask.cancel(true);
			this.warningTask = null;
		}
	}

	@Override
	public void toExecution(Action action) {
		setActionState(action, State.EXECUTION);

		FloatValue value = new FloatValue(1.0D);
		action.addChange(new ValueChange<>(System.currentTimeMillis(), value, ""));
	}

	@Override
	public void toWarning(Action action) {
		setActionState(action, State.WARNING);
	}

	@Override
	public void toExecuted(Action action) {
		cancelWarningTask();
		stop();

		setActionState(action, State.EXECUTED);

		FloatValue value = new FloatValue(0.0D);
		action.addChange(new ValueChange<>(System.currentTimeMillis(), value, ""));
	}

	@Override
	public void toStopped(Action action) {
		cancelWarningTask();
		stop();

		setActionState(action, State.STOPPED);

		FloatValue value = new FloatValue(0.0D);
		action.addChange(new ValueChange<>(System.currentTimeMillis(), value, ""));
	}

	@Override
	public void toError(Action action) {
		cancelWarningTask();
		stop();

		setActionState(action, State.ERROR);

		FloatValue value = new FloatValue(0.0D);
		action.addChange(new ValueChange<>(System.currentTimeMillis(), value, ""));
	}

	protected void addMessage(LogMessage message) {
		if (getContainer().hasComponent(OperationsLog.class)) {
			OperationsLog operationsLog = getContainer().getComponent(OperationsLog.class);
			operationsLog.addMessage(message);
		}
	}

	protected void toExecuted() throws Exception {
		cancelWarningTask();
		stop();
		getController().toExecuted(this.actionLoc);
		getComponent(ExecutionHandler.class).triggerExecution(this.realm);
	}

	protected void toError(LogMessage message) {
		cancelWarningTask();
		stop();
		logger.error("Action " + message.getLocator() + " failed because of: " + message.formatMessage());
		addMessage(message);
		getController().asyncToError(message.getLocator());
	}

	protected void toWarning(LogMessage message) {
		cancelWarningTask();
		addMessage(message);
		getController().asyncToWarning(message.getLocator());
	}

	protected StrolchTransaction openLocalTx(PrivilegeContext ctx, boolean readOnly) throws StrolchException {
		return getContainer().getRealm(ctx.getCertificate()).openTx(ctx.getCertificate(), getClass(), readOnly);
	}

	protected void runWithFreshAction(Consumer<Action> consumer) {
		runWithFreshAction(consumer, true);
	}

	protected void runWithFreshAction(Consumer<Action> consumer, boolean readOnly) {
		try {
			runAsAgent(ctx -> {
				try (StrolchTransaction tx = openLocalTx(ctx, readOnly)) {
					tx.lock(this.actionLoc.trim(3));
					Action action = tx.findElement(this.actionLoc);
					consumer.accept(action);
				}
			});
		} catch (Exception e) {
			logger.error("Failed to perform consumer " + consumer.toString(), e);
		}
	}

	protected void runWithFreshAction(BiConsumer<StrolchTransaction, Action> consumer) {
		runWithFreshAction(true, consumer);
	}

	protected void runWithFreshAction(boolean readOnly, BiConsumer<StrolchTransaction, Action> consumer) {
		try {
			runAsAgent(ctx -> {
				try (StrolchTransaction tx = openLocalTx(ctx, readOnly)) {
					tx.lock(this.actionLoc.trim(3));
					Action action = tx.findElement(this.actionLoc);
					consumer.accept(tx, action);

					if (!readOnly && tx.needsCommit())
						tx.commitOnClose();
				}
			});
		} catch (Exception e) {
			logger.error("Failed to perform consumer " + consumer.toString(), e);
		}
	}
}
