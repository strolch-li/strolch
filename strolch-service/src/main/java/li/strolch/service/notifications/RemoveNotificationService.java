package li.strolch.service.notifications;

import li.strolch.model.Resource;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.service.StringArgument;
import li.strolch.service.api.AbstractService;
import li.strolch.service.api.ServiceResult;
import li.strolch.utils.dbc.DBC;

import static li.strolch.model.StrolchModelConstants.TYPE_NOTIFICATION;

public class RemoveNotificationService extends AbstractService<StringArgument, ServiceResult> {
	@Override
	protected ServiceResult getResultInstance() {
		return new ServiceResult();
	}

	@Override
	public StringArgument getArgumentInstance() {
		return new StringArgument();
	}

	@Override
	protected ServiceResult internalDoService(StringArgument arg) throws Exception {
		DBC.PRE.assertNotEmpty("value must be set", arg.value);

		try (StrolchTransaction tx = openArgOrUserTx(arg)) {
			Resource notification = tx.getResourceBy(TYPE_NOTIFICATION, arg.value, true);
			tx.remove(notification);
			tx.commitOnClose();
		}

		return ServiceResult.success();
	}
}
