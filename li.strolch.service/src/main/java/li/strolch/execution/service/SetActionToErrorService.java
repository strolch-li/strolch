package li.strolch.execution.service;

import li.strolch.execution.Controller;
import li.strolch.execution.ExecutionHandler;
import li.strolch.execution.command.SetActionToErrorCommand;
import li.strolch.model.activity.Action;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.service.LocatorArgument;
import li.strolch.service.api.AbstractService;
import li.strolch.service.api.ServiceResult;
import li.strolch.service.api.ServiceResultState;

public class SetActionToErrorService extends AbstractService<LocatorArgument, ServiceResult> {

	@Override
	protected ServiceResult getResultInstance() {
		return new ServiceResult(ServiceResultState.FAILED);
	}

	@Override
	public LocatorArgument getArgumentInstance() {
		return new LocatorArgument();
	}

	@Override
	protected ServiceResult internalDoService(LocatorArgument arg) throws Exception {

		try (StrolchTransaction tx = openArgOrUserTx(arg)) {

			tx.lock(arg.locator.trim(3));
			Action action = tx.findElement(arg.locator);
			tx.lock(action.getResourceLocator());

			ExecutionHandler executionHandler = getComponent(ExecutionHandler.class);
			Controller controller = executionHandler.getController(tx.getRealmName(), action.getRootElement());
			if (controller != null) {
				controller.toError(action.getLocator());
			} else {

				SetActionToErrorCommand command = new SetActionToErrorCommand(tx);
				command.setAction(action);
				tx.addCommand(command);
			}

			tx.commitOnClose();
		}

		return ServiceResult.success();
	}
}
