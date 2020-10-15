package li.strolch.execution.policy;

import java.util.function.BiConsumer;
import java.util.function.Consumer;

import li.strolch.exception.StrolchException;
import li.strolch.handler.operationslog.OperationsLog;
import li.strolch.model.State;
import li.strolch.model.activity.Action;
import li.strolch.model.log.LogMessage;
import li.strolch.model.timevalue.impl.FloatValue;
import li.strolch.model.timevalue.impl.ValueChange;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.PrivilegeContext;

/**
 * <p>
 * Simple Execution Policy which sets the state of the action depending on the method called.
 * </p>
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class SimpleExecution extends ExecutionPolicy {

	public SimpleExecution(StrolchTransaction tx) {
		super(tx);
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
		stop();

		setActionState(action, State.EXECUTED);

		FloatValue value = new FloatValue(0.0D);
		action.addChange(new ValueChange<>(System.currentTimeMillis(), value, ""));
	}

	@Override
	public void toStopped(Action action) {
		stop();

		setActionState(action, State.STOPPED);

		FloatValue value = new FloatValue(0.0D);
		action.addChange(new ValueChange<>(System.currentTimeMillis(), value, ""));
	}

	@Override
	public void toError(Action action) {
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
		stop();
		getController().toExecuted(this.actionLoc);
	}

	protected void toError(LogMessage message) {
		stop();
		logger.error("Action " + message.getLocator() + " failed because of: " + message.formatMessage());
		addMessage(message);
		getController().asyncToError(message.getLocator());
	}

	protected void toWarning(LogMessage message) {
		stop();
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
		runWithFreshAction(consumer, true);
	}

	protected void runWithFreshAction(BiConsumer<StrolchTransaction, Action> consumer, boolean readOnly) {
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
