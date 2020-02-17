package li.strolch.execution.service;

import static li.strolch.utils.helper.StringHelper.isEmpty;

import java.text.MessageFormat;

import li.strolch.exception.StrolchException;
import li.strolch.execution.ExecutionHandler;
import li.strolch.model.State;
import li.strolch.model.activity.Action;
import li.strolch.model.activity.Activity;
import li.strolch.model.activity.IActivityElement;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.runtime.StrolchConstants;
import li.strolch.service.LocatorArgument;
import li.strolch.service.api.AbstractService;
import li.strolch.service.api.ServiceResult;
import li.strolch.service.api.ServiceResultState;

public class SetToExecutionService extends AbstractService<LocatorArgument, ServiceResult> {

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

		String realm = isEmpty(arg.realm) ? StrolchConstants.DEFAULT_REALM : arg.realm;

		Activity activity;
		try (StrolchTransaction tx = openTx(realm)) {
			tx.lock(arg.locator);

			IActivityElement element = tx.findElement(arg.locator);
			if (element.getState().canNotSetToExecution()) {
				String msg = "Current state is {0} and can not be changed to {1} for action {2}";
				msg = MessageFormat.format(msg, element.getState(), State.EXECUTION, element.getLocator());
				throw new StrolchException(msg);
			}

			if (element.isAction() && element.getState() == State.STOPPED) {
				((Action) element).setState(State.EXECUTABLE);
			}

			activity = element.getRootElement();
		}

		ExecutionHandler executionHandler = getContainer().getComponent(ExecutionHandler.class);
		executionHandler.addForExecution(realm, activity);

		return ServiceResult.success();
	}
}
