package li.strolch.execution.service;

import li.strolch.execution.Controller;
import li.strolch.execution.ExecutionHandler;
import li.strolch.model.State;
import li.strolch.model.activity.Action;
import li.strolch.model.activity.Activity;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.service.LocatorArgument;
import li.strolch.service.api.AbstractService;
import li.strolch.service.api.ServiceResult;
import li.strolch.service.api.ServiceResultState;

public class ExecuteActionService extends AbstractService<LocatorArgument, ServiceResult> {

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
		String realm = getArgOrUserRealm(arg);
		ExecutionHandler executionHandler = getComponent(ExecutionHandler.class);

		Controller controller = executionHandler.getController(realm, arg.locator.trim(3));
		if (controller != null) {
			controller.execute();
			return ServiceResult.success();
		}

		try (StrolchTransaction tx = openTx(realm, true)) {
			tx.lock(arg.locator);
			Activity activity = tx.getActivityBy(arg.locator.get(1), arg.locator.get(2), true);
			executionHandler.toExecution(realm, activity);
		}

		return ServiceResult.success();
	}
}
