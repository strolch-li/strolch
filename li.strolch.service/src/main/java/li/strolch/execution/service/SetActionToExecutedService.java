package li.strolch.execution.service;

import li.strolch.execution.command.SetActionToExecutedCommand;
import li.strolch.model.activity.Action;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.service.LocatorArgument;
import li.strolch.service.api.AbstractService;
import li.strolch.service.api.ServiceResult;
import li.strolch.service.api.ServiceResultState;

public class SetActionToExecutedService extends AbstractService<LocatorArgument, ServiceResult> {

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

			Action action = tx.findElement(arg.locator);

			SetActionToExecutedCommand command = new SetActionToExecutedCommand(getContainer(), tx);
			command.setAction(action);
			tx.addCommand(command);

			tx.commitOnClose();
		}

		return ServiceResult.success();
	}
}
