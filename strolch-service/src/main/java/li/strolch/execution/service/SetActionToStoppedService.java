package li.strolch.execution.service;

import li.strolch.execution.ExecutionHandler;
import li.strolch.execution.command.SetActionToStoppedCommand;
import li.strolch.model.activity.Action;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.service.LocatorArgument;
import li.strolch.service.api.AbstractService;
import li.strolch.service.api.ServiceResult;
import li.strolch.service.api.ServiceResultState;

public class SetActionToStoppedService extends AbstractService<LocatorArgument, ServiceResult> {

	@Override
	protected ServiceResult getResultInstance() {
		return new ServiceResult(ServiceResultState.FAILED);
	}

	@Override
	public LocatorArgument getArgumentInstance() {
		return new LocatorArgument();
	}

	@Override
	protected ServiceResult internalDoService(LocatorArgument arg) {

		ExecutionHandler executionHandler = getComponent(ExecutionHandler.class);
		String realm = getArgOrUserRealm(arg);
		if (executionHandler.isControlling(realm, arg.locator.trim(3))) {
			executionHandler.toStopped(realm, arg.locator);
			return ServiceResult.success();
		}

		try (StrolchTransaction tx = openArgOrUserTx(arg)) {
			tx.lock(arg.locator.trim(3));

			Action action = tx.findElement(arg.locator);
			tx.lock(action.getResourceLocator());

			SetActionToStoppedCommand command = new SetActionToStoppedCommand(tx);
			command.setAction(action);
			tx.addCommand(command);

			tx.commitOnClose();
		}

		return ServiceResult.success();
	}
}
