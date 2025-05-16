/*
 * Copyright (c) 2013-2025 Robert von Burg <eitch@eitchnet.ch>
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

package li.strolch.execution.policy;

import li.strolch.model.State;
import li.strolch.model.activity.Activity;
import li.strolch.model.policy.PolicyDef;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.policy.StrolchPolicy;

/**
 * The {@link ActivityArchivalPolicy} is called when an {@link Activity} has reached the state {@link State#EXECUTED}
 * and can thus be archived. Here the archivation of the {@link Activity} can be implemented, e.g. removing it, or
 * exporting it to a different system etc.
 *
 * @author Robert von Burg &lt;eitch@eitchnet.ch&gt;
 */
public class ActivityArchivalPolicy extends StrolchPolicy {

	public static PolicyDef DEFAULT_ACTIVITY_ARCHIVAL = PolicyDef.valueOf(ActivityArchivalPolicy.class.getSimpleName(),
			"key:DefaultActivityArchival");

	public ActivityArchivalPolicy(StrolchTransaction tx) {
		super(tx);
	}

	public void archive(Activity activity) {
		// do nothing
	}

	@Override
	public void undo() {
		// nothing to undo
	}
}
