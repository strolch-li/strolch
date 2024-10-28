/*
 * Copyright (c) 2024 Robert von Burg <eitch@eitchnet.ch>
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

package li.strolch.policy;

import li.strolch.agent.api.StrolchAgent;
import li.strolch.job.JobMode;
import li.strolch.job.StrolchJob;
import li.strolch.privilege.model.PrivilegeContext;

public class ReloadPoliciesJob extends StrolchJob {

	public ReloadPoliciesJob(StrolchAgent agent, String id, String name, JobMode mode) {
		super(agent, id, name, mode);
	}

	@Override
	protected void execute(PrivilegeContext ctx) {
		PolicyHandler policyHandler = getContainer().getComponent(PolicyHandler.class);
		policyHandler.reloadPolicies();
	}
}
