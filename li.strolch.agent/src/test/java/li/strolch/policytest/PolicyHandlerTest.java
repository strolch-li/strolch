/*
 * Copyright 2015 Robert von Burg <eitch@eitchnet.ch>
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
package li.strolch.policytest;

import static org.junit.Assert.assertNotNull;

import org.junit.Test;

import li.strolch.RuntimeMock;
import li.strolch.agent.ComponentContainerTest;
import li.strolch.agent.api.ComponentContainer;
import li.strolch.model.Resource;
import li.strolch.model.policy.PolicyDef;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.policy.PolicyHandler;
import li.strolch.privilege.model.Certificate;
import li.strolch.runtime.StrolchConstants;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class PolicyHandlerTest {

	public static final String PATH_EMPTY_RUNTIME = "target/PolicyHandlerTest/"; //$NON-NLS-1$

	@Test
	public void shouldInstantiatePolicies() {

		RuntimeMock.runInStrolch(PATH_EMPTY_RUNTIME, ComponentContainerTest.PATH_TRANSIENT_CONTAINER, agent -> {

			PolicyHandler policyHandler = agent.getContainer().getComponent(PolicyHandler.class);

			ComponentContainer container = agent.getContainer();
			Certificate certificate = container.getPrivilegeHandler().authenticate("test", "test".getBytes());
			try (StrolchTransaction tx = container.getRealm(StrolchConstants.DEFAULT_REALM).openTx(certificate,
					"test")) {

				Resource res = tx.getResourceBy("TestType", "MyTestResource");

				PolicyDef planningPolicyDef = res.getPolicyDefs().getPolicyDef("PlanningPolicy");
				TestPlanningPolicy planningPolicy = policyHandler.getPolicy(planningPolicyDef, tx);
				assertNotNull(planningPolicy);

				PolicyDef executionPolicyDef = res.getPolicyDefs().getPolicyDef("ExecutionPolicy");
				TestExecutionPolicy executionPolicy = policyHandler.getPolicy(executionPolicyDef, tx);
				assertNotNull(executionPolicy);

				PolicyDef confirmationPolicyDef = res.getPolicyDefs().getPolicyDef("ConfirmationPolicy");
				TestConfirmationPolicy confirmationPolicy = policyHandler.getPolicy(confirmationPolicyDef, tx);
				assertNotNull(confirmationPolicy);
			}
		});
	}
}
