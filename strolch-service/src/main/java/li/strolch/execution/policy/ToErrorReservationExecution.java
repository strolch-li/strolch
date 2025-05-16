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
import li.strolch.model.log.LogMessage;
import li.strolch.model.log.LogMessageState;
import li.strolch.model.log.LogSeverity;
import li.strolch.persistence.api.StrolchTransaction;

import java.util.ResourceBundle;

import static li.strolch.model.StrolchModelConstants.PolicyConstants.TYPE_RESERVE;

/**
 * The {@link ToErrorReservationExecution} executes same as {@link ReservationExecution} with the difference that
 * {@link
 * #isExecutable(Action)} always returns true, and if the action's resource is currently reserved, the execution fails
 * and the state is set to ERROR
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class ToErrorReservationExecution extends ReservationExecution {

	public ToErrorReservationExecution(StrolchTransaction tx) {
		super(tx);
	}

	/**
	 * In {@link ToErrorReservationExecution} we are always executable, but go to error, if already reserved
	 */
	@Override
	public boolean isExecutable(Action action) {

		if (action.getType().equals(TYPE_RESERVE)) {
			return true;
		}

		return super.isExecutable(action);
	}

	@Override
	public void toExecution(Action action) {

		if (action.getType().equals(TYPE_RESERVE) && isReserved(tx(), action)) {
			setActionState(action, State.EXECUTION);
			toError(new LogMessage(tx().getRealmName(), tx().getCertificate().getUsername(), action.getLocator(),
					LogSeverity.Error, LogMessageState.Information, ResourceBundle.getBundle("strolch-service"),
					"execution.policy.reservation.alreadyReserved").value("resourceLoc",
					action.getResourceLocator().toString()));
		} else {
			super.toExecution(action);
		}
	}
}
