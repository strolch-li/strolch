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

package li.strolch.execution.command;

import li.strolch.exception.StrolchException;
import li.strolch.model.State;
import li.strolch.model.activity.Activity;
import li.strolch.persistence.api.StrolchTransaction;

import java.text.MessageFormat;

public class SetActionToWarningCommand extends ActionExecutionCommand {

	public SetActionToWarningCommand(StrolchTransaction tx) {
		super(tx);
	}

	@Override
	public void validate() {
		super.validate();

		if (!this.action.getState().canSetToWarning()) {
			String msg = "Current state is {0} and can not be changed to {1} for action {2}";
			msg = MessageFormat.format(msg, this.action.getState(), State.WARNING, this.action.getLocator());
			throw new StrolchException(msg);
		}
	}

	@Override
	public void doCommand() {
		if (this.action.getState() == State.WARNING) {
			logger.warn("Action {} is already in state WARNING! Not changing.", this.action.getLocator());
			return;
		}

		Activity rootElement = this.action.getRootElement();
		State currentState = rootElement.getState();

		getExecutionPolicy(this.action).toWarning(this.action);
		getConfirmationPolicy(this.action).toWarning(this.action);

		updateOrderState(tx(), rootElement, currentState, rootElement.getState());
	}
}
