package li.strolch.execution.service;

import li.strolch.execution.ExecutionHandler;
import li.strolch.runtime.StrolchConstants;
import li.strolch.service.api.AbstractService;
import li.strolch.service.api.ServiceArgument;
import li.strolch.service.api.ServiceResult;
import li.strolch.service.api.ServiceResultState;
import li.strolch.utils.helper.StringHelper;

public class ClearAllCurrentExecutionsService extends AbstractService<ServiceArgument, ServiceResult> {

	@Override
	protected ServiceResult getResultInstance() {
		return new ServiceResult(ServiceResultState.FAILED);
	}

	@Override
	public ServiceArgument getArgumentInstance() {
		return new ServiceArgument();
	}

	@Override
	protected ServiceResult internalDoService(ServiceArgument arg) throws Exception {

		String realm = StringHelper.isEmpty(arg.realm) ? StrolchConstants.DEFAULT_REALM : arg.realm;

		ExecutionHandler executionHandler = getContainer().getComponent(ExecutionHandler.class);
		executionHandler.clearAllCurrentExecutions(realm);

		return ServiceResult.success();
	}
}
