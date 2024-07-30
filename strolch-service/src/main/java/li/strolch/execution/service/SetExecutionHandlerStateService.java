package li.strolch.execution.service;

import li.strolch.execution.ExecutionHandler;
import li.strolch.execution.ExecutionHandlerState;
import li.strolch.privilege.model.SimpleRestrictable;
import li.strolch.runtime.StrolchConstants;
import li.strolch.service.StringMapArgument;
import li.strolch.service.api.AbstractService;
import li.strolch.service.api.ServiceResult;
import li.strolch.service.api.ServiceResultState;
import li.strolch.utils.helper.StringHelper;

public class SetExecutionHandlerStateService extends AbstractService<StringMapArgument, ServiceResult> {

	@Override
	protected ServiceResult getResultInstance() {
		return new ServiceResult(ServiceResultState.FAILED);
	}

	@Override
	public StringMapArgument getArgumentInstance() {
		return new StringMapArgument();
	}

	@Override
	protected ServiceResult internalDoService(StringMapArgument arg) {

		String realm = StringHelper.isEmpty(arg.realm) ? StrolchConstants.DEFAULT_REALM : arg.realm;
		String state = arg.map.get("state");

		// validate user can perform this action
		getPrivilegeContext().validateAction(new SimpleRestrictable(getPrivilegeValue(), state));

		switch (state) {
		case "Running" -> {
			ExecutionHandler executionHandler = getContainer().getComponent(ExecutionHandler.class);
			executionHandler.getExecutionState(getCertificate(), realm, ExecutionHandlerState.Running);
			executionHandler.triggerExecution(realm);
		}
		case "HaltNew" -> {
			ExecutionHandler executionHandler = getContainer().getComponent(ExecutionHandler.class);
			executionHandler.getExecutionState(getCertificate(), realm, ExecutionHandlerState.HaltNew);
		}
		case "Paused" -> {
			ExecutionHandler executionHandler = getContainer().getComponent(ExecutionHandler.class);
			executionHandler.getExecutionState(getCertificate(), realm, ExecutionHandlerState.Paused);
		}
		case "Trigger" -> {
			ExecutionHandler executionHandler = getContainer().getComponent(ExecutionHandler.class);
			executionHandler.triggerExecution(realm);
		}
		case "ReloadActivities" -> {
			ExecutionHandler executionHandler = getContainer().getComponent(ExecutionHandler.class);
			executionHandler.reloadActivitiesInExecution(getPrivilegeContext(), realm);
		}
		default -> throw new UnsupportedOperationException("Unhandled state " + state);
		}

		return ServiceResult.success();
	}
}
