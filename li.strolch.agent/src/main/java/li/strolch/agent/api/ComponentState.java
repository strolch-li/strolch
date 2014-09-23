/*
 * Copyright 2013 Robert von Burg <eitch@eitchnet.ch>
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
package li.strolch.agent.api;

import java.text.MessageFormat;

public enum ComponentState {

	UNDEFINED, SETUP, INITIALIZED, STARTED, STOPPED, DESTROYED;

	public ComponentState validateStateChange(ComponentState newState) {

		if (this == newState)
			return this;

		switch (this) {
		case UNDEFINED:
			if (newState != ComponentState.SETUP && newState != STOPPED)
				throw getIllegalStateEx(newState);
			break;
		case SETUP:
			if (newState != ComponentState.INITIALIZED && newState != STOPPED && newState != DESTROYED)
				throw getIllegalStateEx(newState);
			break;
		case INITIALIZED:
			if (newState != ComponentState.STARTED && newState != STOPPED && newState != DESTROYED)
				throw getIllegalStateEx(newState);
			break;
		case STARTED:
			if (newState != ComponentState.STOPPED)
				throw getIllegalStateEx(newState);
			break;
		case STOPPED:
			if (newState != ComponentState.STARTED && newState != ComponentState.DESTROYED)
				throw getIllegalStateEx(newState);
			break;
		case DESTROYED:
			throw getIllegalStateEx(newState);
		default:
			throw getIllegalStateEx(newState);
		}

		return newState;
	}

	private IllegalStateException getIllegalStateEx(ComponentState newState) {
		String msg = "Moving from state {0} to state {1} is not allowed!"; //$NON-NLS-1$
		msg = MessageFormat.format(msg, this, newState);
		throw new IllegalStateException(msg);
	}
}
