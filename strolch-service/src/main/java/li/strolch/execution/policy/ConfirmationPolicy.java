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

import li.strolch.model.activity.Action;
import li.strolch.model.policy.PolicyDef;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.policy.StrolchPolicy;

/**
 * The {@link ConfirmationPolicy} is called for every state change of an {@link Action}. This is where extra tasks can
 * be performed when an {@link Action} enters a specific state, e.g. send an e-mail, etc.
 *
 * @author Robert von Burg &lt;eitch@eitchnet.ch&gt;
 */
public class ConfirmationPolicy extends StrolchPolicy {

	public static final PolicyDef DEFAULT_CONFIRMATION = PolicyDef.valueOf(ConfirmationPolicy.class.getSimpleName(),
			"key:DefaultConfirmation");

	public ConfirmationPolicy(StrolchTransaction tx) {
		super(tx);
	}

	public void toCreated(Action action) {
		// do nothing
	}

	public void toPlanning(Action action) {
		// do nothing
	}

	public void toPlanned(Action action) {
		// do nothing
	}

	public void toExecution(Action action) {
		// do nothing
	}

	public void toStopped(Action action) {
		// do nothing
	}

	public void toWarning(Action action) {
		// do nothing
	}

	public void toError(Action action) {
		// do nothing
	}

	public void toExecuted(Action action) {
		// do nothing
	}

	public void toClosed(Action action) {
		// do nothing
	}

	/**
	 * Calls the appropriate confirmation method depending on the state of the {@link Action}
	 *
	 * @param action the action for which to perform the confirmation call
	 */
	public void doConfirmation(Action action) {
		switch (action.getState()) {
			case CREATED -> toCreated(action);
			case PLANNING -> toPlanning(action);
			case PLANNED -> toPlanned(action);
			case EXECUTION -> toExecution(action);
			case WARNING -> toWarning(action);
			case ERROR -> toError(action);
			case STOPPED -> toStopped(action);
			case EXECUTED -> toExecuted(action);
			case CLOSED -> toClosed(action);
			case EXECUTABLE -> {
				// do nothing
			}
		}
	}
}
