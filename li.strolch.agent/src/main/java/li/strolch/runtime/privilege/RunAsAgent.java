package li.strolch.runtime.privilege;

import ch.eitchnet.privilege.handler.SystemUserAction;
import ch.eitchnet.privilege.model.PrivilegeContext;
import li.strolch.service.api.AbstractService;
import li.strolch.service.api.ServiceArgument;
import li.strolch.service.api.ServiceHandler;
import li.strolch.service.api.ServiceResult;

public class RunAsAgent<T extends ServiceArgument, U extends ServiceResult> extends SystemUserAction {

	private ServiceHandler svcHandler;
	private AbstractService<T, U> svc;
	private T arg;
	private U result;

	public RunAsAgent(ServiceHandler svcHandler, AbstractService<T, U> svc, T arg) {
		this.svcHandler = svcHandler;
		this.svc = svc;
		this.arg = arg;
	}

	public U getResult() {
		return result;
	}

	@Override
	public void execute(PrivilegeContext privilegeContext) {
		this.result = svcHandler.doService(privilegeContext.getCertificate(), svc, arg);
	}
}
