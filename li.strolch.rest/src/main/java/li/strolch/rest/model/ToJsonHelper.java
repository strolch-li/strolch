package li.strolch.rest.model;

import static li.strolch.model.Tags.Json.EXECUTION_POLICY;

import li.strolch.execution.Controller;
import li.strolch.execution.ExecutionHandler;
import li.strolch.execution.policy.ExecutionPolicy;
import li.strolch.model.json.StrolchElementToJsonVisitor;

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
