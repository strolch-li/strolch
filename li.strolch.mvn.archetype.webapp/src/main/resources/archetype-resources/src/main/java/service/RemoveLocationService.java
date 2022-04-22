package ${package}.service;

import static ${package}.model.Constants.TYPE_LOCATION;

import li.strolch.model.Resource;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.service.StringServiceArgument;
import li.strolch.service.api.AbstractService;
import li.strolch.service.api.ServiceResult;

public class RemoveLocationService extends AbstractService<StringServiceArgument, ServiceResult> {

	@Override
	protected ServiceResult getResultInstance() {
		return new ServiceResult();
	}

	@Override
	public StringServiceArgument getArgumentInstance() {
		return new StringServiceArgument();
	}

	@Override
	protected ServiceResult internalDoService(StringServiceArgument arg) throws Exception {

		// open a new transaction, using the realm from the argument, or the certificate
		try (StrolchTransaction tx = openArgOrUserTx(arg)) {

			// get the existing book
			Resource location = tx.getResourceBy(TYPE_LOCATION, arg.value, true);

			// save changes
			tx.remove(location);

			// notify the TX that it should commit on close
			tx.commitOnClose();
		}

		// and return the result
		return ServiceResult.success();
	}
}
