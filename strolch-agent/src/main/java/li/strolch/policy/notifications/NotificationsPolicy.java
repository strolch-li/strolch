/*
 * Copyright (c) 2013-2024 Robert von Burg <eitch@eitchnet.ch>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

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
