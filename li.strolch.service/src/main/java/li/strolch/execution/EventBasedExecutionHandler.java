package li.strolch.execution;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.execution.command.ExecuteActivityCommand;
import li.strolch.execution.command.SetActionToErrorCommand;
import li.strolch.execution.command.SetActionToExecutedCommand;
import li.strolch.execution.command.SetActionToStoppedCommand;
import li.strolch.execution.command.SetActionToWarningCommand;
import li.strolch.model.Locator;
import li.strolch.model.activity.Action;
import li.strolch.model.activity.Activity;
import li.strolch.model.activity.IActivityElement;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Certificate;
import li.strolch.privilege.model.PrivilegeContext;

public class EventBasedExecutionHandler extends ExecutionHandler {

	public EventBasedExecutionHandler(ComponentContainer container, String componentName) {
		super(container, componentName);
	}

	@Override
	public void toExecution(String realm, Locator locator) {
		runAsAgent(ctx -> {
			toExecution(realm, locator, ctx);
		});
	}

	@Override
	public void toExecuted(String realm, Locator locator) {
		runAsAgent(ctx -> {
			toExecuted(realm, locator, ctx);
		});
	}

	@Override
	public void toStopped(String realm, Locator locator) {
		runAsAgent(ctx -> {
			toStopped(realm, locator, ctx);
		});
	}

	@Override
	public void toError(String realm, Locator locator) {
		runAsAgent(ctx -> {
			toError(realm, locator, ctx);
		});
	}

	@Override
	public void toWarning(String realm, Locator locator) {
		runAsAgent(ctx -> {
			toWarning(realm, locator, ctx);
		});
	}

	private void toExecution(String realm, Locator locator, PrivilegeContext ctx) {
		try (StrolchTransaction tx = openTx(realm, ctx.getCertificate(), ExecuteActivityCommand.class)) {
			IActivityElement activityElement = tx.findElement(locator);
			Activity activity = activityElement.getRootElement();

			ExecuteActivityCommand command = new ExecuteActivityCommand(getContainer(), tx);
			command.setActivity(activity);
			tx.addCommand(command);

			tx.commitOnClose();
		}
	}

	private void toExecuted(String realm, Locator locator, PrivilegeContext ctx) {

		Locator activityLoc;

		try (StrolchTransaction tx = openTx(realm, ctx.getCertificate(), SetActionToExecutedCommand.class)) {
			Action action = tx.findElement(locator);
			activityLoc = action.getRootElement().getLocator();

			SetActionToExecutedCommand command = new SetActionToExecutedCommand(getContainer(), tx);
			command.setAction(action);
			tx.addCommand(command);

			tx.commitOnClose();
		}

		toExecution(realm, activityLoc, ctx);
	}

	private void toWarning(String realm, Locator locator, PrivilegeContext ctx) {
		try (StrolchTransaction tx = openTx(realm, ctx.getCertificate(), SetActionToExecutedCommand.class)) {
			Action action = tx.findElement(locator);

			SetActionToWarningCommand command = new SetActionToWarningCommand(getContainer(), tx);
			command.setAction(action);
			tx.addCommand(command);

			tx.commitOnClose();
		}
	}

	private void toError(String realm, Locator locator, PrivilegeContext ctx) {
		try (StrolchTransaction tx = openTx(realm, ctx.getCertificate(), SetActionToExecutedCommand.class)) {
			Action action = tx.findElement(locator);

			SetActionToErrorCommand command = new SetActionToErrorCommand(getContainer(), tx);
			command.setAction(action);
			tx.addCommand(command);

			tx.commitOnClose();
		}
	}

	private void toStopped(String realm, Locator locator, PrivilegeContext ctx) {
		try (StrolchTransaction tx = openTx(realm, ctx.getCertificate(), SetActionToStoppedCommand.class)) {
			Action action = tx.findElement(locator);

			SetActionToStoppedCommand command = new SetActionToStoppedCommand(getContainer(), tx);
			command.setAction(action);
			tx.addCommand(command);

			tx.commitOnClose();
		}
	}

	protected StrolchTransaction openTx(String realm, Certificate cert, Class<?> clazz) {
		return getContainer().getRealm(realm).openTx(cert, clazz);
	}
}
