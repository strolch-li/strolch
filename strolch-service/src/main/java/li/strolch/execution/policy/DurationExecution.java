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
import li.strolch.model.activity.Action;
import li.strolch.persistence.api.StrolchTransaction;

import java.util.concurrent.TimeUnit;

import static li.strolch.model.StrolchModelConstants.PolicyConstants.PARAM_DURATION;

/**
 * <p>
 * Simple Execution Policy which starts the execution immediately, i.e. set state to in execution and completes after
 * the {@link Action Action's} duration has passed.
 * </p>
 *
 * <p>Sub classes can by pass the delaying of completion by directly calling
 * {@link #setActionStateWithValueChange(Action, State, double)}. This is useful when calling any of the
 * {@link #delayRandom(long, TimeUnit, Runnable)} methods</p>
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class DurationExecution extends SimpleExecution {

	public DurationExecution(StrolchTransaction tx) {
		super(tx);
	}

	/**
	 * Delays completion of the given action.
	 *
	 * <p>Sub classes can by pass the delaying of completion by directly calling
	 * {@link #setActionStateWithValueChange(Action, State, double)}</p>
	 *
	 * @param action the action to start execution for
	 */
	@Override
	public void toExecution(Action action) {
		super.toExecution(action);
		if (action.findObjectivesParam(PARAM_DURATION, true).isEmpty())
			toExecuted(action);
		else
			delayToExecuted(action);
	}
}
