package li.strolch.execution.service;

import java.text.MessageFormat;

import li.strolch.exception.StrolchException;
import li.strolch.execution.ExecutionHandler;
import li.strolch.execution.command.*;
import li.strolch.model.Locator;
import li.strolch.model.State;
import li.strolch.model.activity.Action;
import li.strolch.model.activity.IActivityElement;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.service.StringMapArgument;
import li.strolch.service.api.AbstractService;
import li.strolch.service.api.ServiceResult;
import li.strolch.service.api.ServiceResultState;

public class SetActionStateService extends AbstractService<StringMapArgument, ServiceResult> {

	@Override
	protected ServiceResult getResultInstance() {
		return new ServiceResult(ServiceResultState.FAILED);
	}

	@Override
	public StringMapArgument getArgumentInstance() {
		return new StringMapArgument();
	}

	@Override
	protected ServiceResult internalDoService(StringMapArgument arg) throws Exception {

		State state = State.parse(arg.map.get("state"));
		Locator locator = Locator.valueOf(arg.map.get("locator"));

		String realm;
		try (StrolchTransaction tx = openArgOrUserTx(arg)) {
			realm = tx.getRealmName();

			Action action = tx.findElement(locator);

			switch (state) {
			case CREATED: {

				SetActionToCreatedCommand command = new SetActionToCreatedCommand(getContainer(), tx);
				command.setAction(action);
				tx.addCommand(command);

				break;
			}

			case PLANNING: {

				SetActionToPlanningCommand command = new SetActionToPlanningCommand(getContainer(), tx);
				command.setAction(action);
				tx.addCommand(command);

				break;
			}

			case PLANNED: {

				SetActionToPlannedCommand command = new SetActionToPlannedCommand(getContainer(), tx);
				command.setAction(action);
				tx.addCommand(command);

				break;
			}

			case EXECUTION: {

				tx.lock(locator);

				IActivityElement element = tx.findElement(locator);
				if (!element.getState().canSetToExecution()) {
					String msg = "Current state is {0} and can not be changed to {1} for action {2}";
					msg = MessageFormat.format(msg, element.getState(), State.EXECUTION, element.getLocator());
					throw new StrolchException(msg);
				}

				ExecutionHandler executionHandler = getContainer().getComponent(ExecutionHandler.class);
				executionHandler.toExecution(tx.getRealmName(), locator);

				break;
			}

			case WARNING: {

				SetActionToWarningCommand command = new SetActionToWarningCommand(getContainer(), tx);
				command.setAction(action);
				tx.addCommand(command);

				break;
			}

			case ERROR: {

				SetActionToErrorCommand command = new SetActionToErrorCommand(getContainer(), tx);
				command.setAction(action);
				tx.addCommand(command);

				break;
			}

			case STOPPED: {

				SetActionToStoppedCommand command = new SetActionToStoppedCommand(getContainer(), tx);
				command.setAction(action);
				tx.addCommand(command);

				break;
			}

			case EXECUTED: {

				SetActionToExecutedCommand command = new SetActionToExecutedCommand(getContainer(), tx);
				command.setAction(action);
				tx.addCommand(command);

				break;
			}

			case CLOSED: {

				SetActionToClosedCommand command = new SetActionToClosedCommand(getContainer(), tx);
				command.setAction(action);
				tx.addCommand(command);

				break;
			}

			default:
				throw new UnsupportedOperationException("Unhandled state " + state);
			}

			tx.commitOnClose();
		}

		ExecutionHandler executionHandler = getContainer().getComponent(ExecutionHandler.class);
		executionHandler.triggerExecution(realm);

		return ServiceResult.success();
	}
}
