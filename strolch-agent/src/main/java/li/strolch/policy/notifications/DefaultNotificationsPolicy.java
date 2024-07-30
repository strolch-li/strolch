package li.strolch.policy.notifications;

import li.strolch.model.Resource;
import li.strolch.model.StrolchModelConstants;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.utils.collections.DateRange;

import java.time.ZonedDateTime;
import java.util.List;

import static li.strolch.model.StrolchModelConstants.*;

public class DefaultNotificationsPolicy extends NotificationsPolicy {
	public DefaultNotificationsPolicy(StrolchTransaction tx) {
		super(tx);
	}

	@Override
	public List<Resource> findUserNotifications() {
		return tx().streamResources(StrolchModelConstants.TYPE_NOTIFICATION).filter(this::isForUser).toList();
	}

	@Override
	public boolean canView(Resource notification) {
		return isForAll(notification) || isForRole(notification) || isForGroup(notification);
	}

	protected boolean isForUser(Resource notification) {
		if (!isActive(notification))
			return false;
		return isEnabled(notification) && (
				isForAll(notification) || isForRole(notification) || isForGroup(notification));
	}

	protected boolean isActive(Resource notification) {
		return new DateRange()
				.from(notification.getDate(BAG_VISIBILITY, PARAM_VISIBLE_FROM), true)
				.to(notification.getDate(BAG_VISIBILITY, PARAM_VISIBLE_TO), true)
				.contains(ZonedDateTime.now());
	}

	protected boolean isEnabled(Resource notification) {
		return notification.getBoolean(BAG_VISIBILITY, PARAM_ENABLED);
	}

	protected boolean isForAll(Resource notification) {
		return notification.getBoolean(BAG_VISIBILITY, PARAM_FOR_ALL);
	}

	protected boolean isForRole(Resource notification) {
		List<String> roles = notification.getStringList(BAG_VISIBILITY, PARAM_ROLES);
		return roles.stream().anyMatch(r -> tx().getCertificate().hasRole(r));
	}

	protected boolean isForGroup(Resource notification) {
		List<String> groups = notification.getStringList(BAG_VISIBILITY, PARAM_GROUPS);
		return groups.stream().anyMatch(r -> tx().getCertificate().hasGroup(r));
	}
}
