package li.strolch.policy.notifications;

import li.strolch.model.Resource;
import li.strolch.model.policy.PolicyDef;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.policy.StrolchPolicy;

import java.util.List;

import static li.strolch.model.StrolchModelConstants.PolicyConstants.POLICY_DEFAULT;
import static li.strolch.model.policy.PolicyDef.getJavaPolicy;
import static li.strolch.model.policy.PolicyDef.getKeyPolicy;

public abstract class NotificationsPolicy extends StrolchPolicy {

	public NotificationsPolicy(StrolchTransaction tx) {
		super(tx);
	}

	public abstract List<Resource> findUserNotifications();
	public abstract boolean canView(Resource notification);

	public static NotificationsPolicy getDefaultPolicy(StrolchTransaction tx) {
		PolicyDef defaultDef = getKeyPolicy(NotificationsPolicy.class, POLICY_DEFAULT);
		PolicyDef fallbackDef = getJavaPolicy(NotificationsPolicy.class, DefaultNotificationsPolicy.class);
		return tx.getPolicy(NotificationsPolicy.class, defaultDef, fallbackDef);
	}
}
