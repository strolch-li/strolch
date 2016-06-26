package li.strolch.runtime.privilege;

import li.strolch.privilege.handler.SystemUserAction;
import li.strolch.privilege.model.PrivilegeContext;
import li.strolch.service.api.AbstractService;
import li.strolch.service.api.Service;
import li.strolch.service.api.ServiceArgument;
import li.strolch.service.api.ServiceHandler;
import li.strolch.service.api.ServiceResult;

/**
 * {@link SystemUserAction} to run a {@link Service} as a system user
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 *
 * @param <T>
 * @param <U>
 */
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
