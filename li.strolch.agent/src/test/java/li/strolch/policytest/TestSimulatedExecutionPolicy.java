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

import li.strolch.agent.api.ComponentContainer;
import li.strolch.model.State;
import li.strolch.model.activity.Action;
import li.strolch.persistence.api.StrolchTransaction;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class TestSimulatedExecutionPolicy extends TestExecutionPolicy {

	public TestSimulatedExecutionPolicy(ComponentContainer container, StrolchTransaction tx) {
		super(container, tx);
	}

	@Override
	public void execute(Action action) {
		action.setState(State.EXECUTION);
	}

	@Override
	public void undo() {
		// do nothing
	}
}
