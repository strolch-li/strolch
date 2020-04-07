package li.strolch.execution.service;

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

		String realm;
		Activity activity;
		try (StrolchTransaction tx = openArgOrUserTx(arg)) {
			realm = tx.getRealmName();

			tx.lock(arg.locator.trim(3));

			if (arg.locator.getSize() == 3) {
				activity = tx.findElement(arg.locator);
			} else {

				Action action = tx.findElement(arg.locator);

				// this is so we can re-execute stopped actions
				if (action.getState() == State.STOPPED) {
					action.setState(State.EXECUTABLE);

					tx.update(action.getRootElement());
					tx.commitOnClose();
				}

				activity = action.getRootElement();
			}
		}

		getComponent(ExecutionHandler.class).toExecution(realm, activity);

		return ServiceResult.success();
	}
}
