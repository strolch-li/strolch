package li.strolch.execution.service;

import static li.strolch.service.I18nService.i18nService;

import li.strolch.execution.ExecutionHandler;
import li.strolch.execution.ExecutionHandlerState;
import li.strolch.runtime.StrolchConstants;
import li.strolch.service.LocatorArgument;
import li.strolch.service.StrolchRootElementResult;
import li.strolch.service.api.AbstractService;
import li.strolch.service.api.ServiceResult;
import li.strolch.service.api.ServiceResultState;
import li.strolch.utils.helper.StringHelper;

public class StartActivityExecutionService extends AbstractService<LocatorArgument, ServiceResult> {

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

		String realm = StringHelper.isEmpty(arg.realm) ? StrolchConstants.DEFAULT_REALM : arg.realm;

		ExecutionHandler executionHandler = getContainer().getComponent(ExecutionHandler.class);
		ExecutionHandlerState executionHandlerState = executionHandler.getState(getRealmName());
		if (executionHandlerState != ExecutionHandlerState.Running)
			return new StrolchRootElementResult(ServiceResultState.WARNING,
					"ExecutionHandler is not running, can not start new jobs!")
					.i18n(i18nService, "execution.handler.invalidState", "state", executionHandlerState);

		executionHandler.addForExecution(realm, arg.locator);

		return ServiceResult.success();
	}
}
