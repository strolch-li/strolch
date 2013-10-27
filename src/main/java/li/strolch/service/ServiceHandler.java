package li.strolch.service;

import ch.eitchnet.privilege.model.Certificate;

public interface ServiceHandler {

	public <T extends ServiceArgument> ServiceResult doService(Certificate certificate, Service<T> service, T argument);
}