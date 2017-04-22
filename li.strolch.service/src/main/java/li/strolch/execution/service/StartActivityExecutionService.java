package li.strolch.execution.service;

import li.strolch.execution.ExecutionHandler;
import li.strolch.runtime.StrolchConstants;
import li.strolch.service.LocatorArgument;
import li.strolch.service.api.AbstractService;
import li.strolch.service.api.ServiceResult;
import li.strolch.service.api.ServiceResultState;
import li.strolch.utils.helper.StringHelper;

public class StartActivityExecutionService extends AbstractService<LocatorArgument, ServiceResult> {
	private static final long serialVersionUID = 1L;

	@Override
	protected ServiceResult getResultInstance() {
		return new ServiceResult(ServiceResultState.FAILED);
	}

	@Override
	protected ServiceResult internalDoService(LocatorArgument arg) throws Exception {

		String realm = StringHelper.isEmpty(arg.realm) ? StrolchConstants.DEFAULT_REALM : arg.realm;

		ExecutionHandler executionHandler = getContainer().getComponent(ExecutionHandler.class);
		executionHandler.addForExecution(realm, arg.locator);

		return ServiceResult.success();
	}
}
