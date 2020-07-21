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
	protected ServiceResult internalDoService(StringMapArgument arg) throws Exception {

		String realm = StringHelper.isEmpty(arg.realm) ? StrolchConstants.DEFAULT_REALM : arg.realm;
		String state = arg.map.get("state");

		// validate user can perform this action
		getPrivilegeContext().validateAction(new SimpleRestrictable(getPrivilegeValue(), state));

		switch (state) {
		case "Running": {

			ExecutionHandler executionHandler = getContainer().getComponent(ExecutionHandler.class);
			executionHandler.setState(getCertificate(), realm, ExecutionHandlerState.Running);
			executionHandler.triggerExecution(realm);

			break;
		}
		case "HaltNew": {

			ExecutionHandler executionHandler = getContainer().getComponent(ExecutionHandler.class);
			executionHandler.setState(getCertificate(), realm, ExecutionHandlerState.HaltNew);

			break;
		}
		case "Paused": {

			ExecutionHandler executionHandler = getContainer().getComponent(ExecutionHandler.class);
			executionHandler.setState(getCertificate(), realm, ExecutionHandlerState.Paused);

			break;
		}
		case "Trigger": {

			ExecutionHandler executionHandler = getContainer().getComponent(ExecutionHandler.class);
			executionHandler.triggerExecution(realm);

			break;
		}
		case "ReloadActivities": {

			ExecutionHandler executionHandler = getContainer().getComponent(ExecutionHandler.class);
			executionHandler.reloadActivitiesInExecution(getPrivilegeContext(), realm);

			break;
		}

		default:
			throw new UnsupportedOperationException("Unhandled state " + state);
		}

		return ServiceResult.success();
	}
}
