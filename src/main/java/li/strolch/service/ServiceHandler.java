package li.strolch.service;

import ch.eitchnet.privilege.model.Certificate;

public interface ServiceHandler {

	public <T extends ServiceArgument, U extends ServiceResult> U doService(Certificate certificate,
			Service<T, U> service, T argument);

	public <U extends ServiceResult> U doService(Certificate certificate, Service<ServiceArgument, U> service);
}