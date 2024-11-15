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

package li.strolch.rest.model;

import li.strolch.execution.Controller;
import li.strolch.execution.ExecutionHandler;
import li.strolch.execution.policy.ExecutionPolicy;
import li.strolch.model.json.StrolchElementToJsonVisitor;

import static li.strolch.model.Tags.Json.EXECUTION_POLICY;

public class ToJsonHelper {

	public static StrolchElementToJsonVisitor inExecutionActivityToJson(String realmName,
			ExecutionHandler executionHandler) {

		StrolchElementToJsonVisitor visitor = new StrolchElementToJsonVisitor().withVersion().withLocator();
		return visitor.actionHook((action, actionJ) -> {
			if (action.inCreatedPhase()) {
				actionJ.addProperty(EXECUTION_POLICY, "-");
				return;
			}

			Controller controller = executionHandler.getController(realmName, action.getRootElement().getLocator());
			if (controller == null) {
				actionJ.addProperty(EXECUTION_POLICY, "-");
				return;
			}

			ExecutionPolicy executionPolicy = controller.getExecutionPolicy(action.getLocator());
			actionJ.addProperty(EXECUTION_POLICY, executionPolicy == null ? "-" : executionPolicy.getClass().getName());
		});
	}
}
