package li.strolch.execution.service;

import li.strolch.execution.command.SetActionToPlannedCommand;
import li.strolch.model.activity.Action;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.service.LocatorArgument;
import li.strolch.service.api.AbstractService;
import li.strolch.service.api.ServiceResult;
import li.strolch.service.api.ServiceResultState;

public class SetActionToPlannedService extends AbstractService<LocatorArgument, ServiceResult> {

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

		try (StrolchTransaction tx = openArgOrUserTx(arg)) {
			tx.lock(arg.locator.trim(3));

			Action action = tx.findElement(arg.locator);
			if (action.isResourceDefined())
				tx.lock(action.getResourceLocator());

			SetActionToPlannedCommand command = new SetActionToPlannedCommand(tx);
			command.setAction(action);
			tx.addCommand(command);

			tx.commitOnClose();
		}

		return ServiceResult.success();
	}
}
