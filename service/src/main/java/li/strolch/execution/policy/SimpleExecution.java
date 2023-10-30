package li.strolch.execution.policy;

import li.strolch.exception.StrolchException;
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

import java.util.Locale;
import java.util.concurrent.ScheduledFuture;
import java.util.function.BiConsumer;
import java.util.function.Supplier;

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
			logger.warn("There is already a warning task registered, for action " + action.getLocator() +
					". Cancelling and creating a new task...");
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
		setActionStateWithValueChange(action, State.EXECUTION, 1.0D);
	}

	@Override
	public void toStopped(Action action) {
		cancelWarningTask();
		stop();
		setActionStateWithValueChange(action, State.STOPPED, 0.0D);
	}

	protected void toWarning(Action action, LogMessage message) {
		addMessage(message);
		toWarning(action);
	}

	@Override
	public void toWarning(Action action) {
		cancelWarningTask();
		setActionState(action, State.WARNING);
	}

	protected void toWarning(LogMessage message) {
		cancelWarningTask();
		addMessage(message);
		getExecutionHandler().toWarning(this.realm, this.actionLoc);
	}

	protected void toExecuted() throws Exception {
		cancelWarningTask();
		stop();
		getExecutionHandler().toExecuted(this.realm, this.actionLoc);
	}

	@Override
	public void toExecuted(Action action) {
		cancelWarningTask();
		stop();
		setActionStateWithValueChange(action, State.EXECUTED, 0.0D);
	}

	protected void toError(Action action, LogMessage message) {
		logger.error("Action " + message.getLocator() + " failed because of: " + message.formatMessage());
		addMessage(message);
		toError(action);
	}

	@Override
	public void toError(Action action) {
		cancelWarningTask();
		stop();
		setActionStateWithValueChange(action, State.ERROR, 0.0D);
	}

	protected void toError(LogMessage message) {
		cancelWarningTask();
		stop();
		logger.error("Action " + message.getLocator() + " failed because of: " + message.formatMessage());
		addMessage(message);
		getExecutionHandler().toError(this.realm, this.actionLoc);
	}

	protected void setActionStateWithValueChange(Action action, State execution, double value) {
		setActionState(action, execution);
		action.addChange(new ValueChange<>(System.currentTimeMillis(), new FloatValue(value), ""));
	}

	protected void addMessage(LogMessage message) {
		switch (message.getSeverity()) {
			case Info, Notification -> logger.info(message.getMessage(Locale.getDefault()));
			case Warning -> logger.warn(message.getMessage(Locale.getDefault()));
			case Error, Exception -> logger.error(message.getMessage(Locale.getDefault()));
		}
		if (getContainer().hasComponent(OperationsLog.class)) {
			OperationsLog operationsLog = getContainer().getComponent(OperationsLog.class);
			operationsLog.addMessage(message);
		}
	}

	protected StrolchTransaction openLocalTx(PrivilegeContext ctx, boolean readOnly) throws StrolchException {
		return getContainer().getRealm(ctx.getCertificate()).openTx(ctx.getCertificate(), getClass(), readOnly);
	}

	protected void runWithFreshActionReadonly(BiConsumer<StrolchTransaction, Action> consumer,
			Supplier<String> failMsgSupplier) {
		runWithFreshAction(true, consumer, failMsgSupplier);
	}

	protected void runWithFreshActionWritable(BiConsumer<StrolchTransaction, Action> consumer,
			Supplier<String> failMsgSupplier) {
		runWithFreshAction(false, consumer, failMsgSupplier);
	}

	private void runWithFreshAction(boolean readOnly, BiConsumer<StrolchTransaction, Action> consumer,
			Supplier<String> failMsgSupplier) {
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
			logger.error(failMsgSupplier.get(), e);
		}
	}
}
