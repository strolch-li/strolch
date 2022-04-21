package ${package}.service;

import ${package}.model.Constants;
import li.strolch.model.Resource;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.service.StringServiceArgument;
import li.strolch.service.api.AbstractService;
import li.strolch.service.api.ServiceResult;

public class RemoveBookService extends AbstractService<StringServiceArgument, ServiceResult> {

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
			Resource book = tx.getResourceBy(Constants.TYPE_BOOK, arg.value, true);

			// save changes
			tx.remove(book);

			// notify the TX that it should commit on close
			tx.commitOnClose();
		}

		// and return the result
		return ServiceResult.success();
	}
}
