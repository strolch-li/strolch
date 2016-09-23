package li.strolch.execution.service;

import li.strolch.execution.command.ExecuteActivityCommand;
import li.strolch.model.activity.Activity;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.service.LocatorArgument;
import li.strolch.service.api.AbstractService;
import li.strolch.service.api.ServiceResult;
import li.strolch.service.api.ServiceResultState;

public class ExecuteActivityService extends AbstractService<LocatorArgument, ServiceResult> {
	private static final long serialVersionUID = 1L;

	@Override
	protected ServiceResult getResultInstance() {
		return new ServiceResult(ServiceResultState.FAILED);
	}

	@Override
	protected ServiceResult internalDoService(LocatorArgument arg) throws Exception {

		try (StrolchTransaction tx = openArgOrUserTx(arg)) {

			Activity activity = tx.findElement(arg.locator);

			ExecuteActivityCommand command = new ExecuteActivityCommand(getContainer(), tx);
			command.setActivity(activity);
			tx.addCommand(command);

			tx.commitOnClose();
		}

		return ServiceResult.success();
	}
}
