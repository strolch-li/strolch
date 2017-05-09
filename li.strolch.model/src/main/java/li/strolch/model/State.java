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
package li.strolch.model;

import java.util.Iterator;
import java.util.Map.Entry;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import li.strolch.exception.StrolchException;
import li.strolch.model.activity.Activity;
import li.strolch.model.activity.IActivityElement;
import li.strolch.utils.dbc.DBC;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public enum State {

	CREATED("Created"), //$NON-NLS-1$
	PLANNING("Planning"), //$NON-NLS-1$
	PLANNED("Planned"), //$NON-NLS-1$
	EXECUTION("Execution"), //$NON-NLS-1$
	STOPPED("Stopped"), //$NON-NLS-1$
	WARNING("Warning"), //$NON-NLS-1$
	ERROR("Error"), //$NON-NLS-1$
	EXECUTED("Executed"), //$NON-NLS-1$
	CLOSED("Closed"); //$NON-NLS-1$

	private static final Logger logger = LoggerFactory.getLogger(State.class);

	private String state;

	private State(String state) {
		this.state = state;
	}

	public String getName() {
		return this.state;
	}

	/**
	 * @return true if the state is {@link #CREATED}
	 */
	public boolean inCreatedPhase() {
		return this == CREATED;
	}

	/**
	 * @return true if the state is one of {@link #PLANNING} or {@link #PLANNED}
	 */
	public boolean inPlanningPhase() {
		return this == PLANNING || this == PLANNED;
	}

	/**
	 * @return true if the state is one of {@link #EXECUTION}, {@link #STOPPED}, {@link #WARNING}, {@link #ERROR} or
	 *         {@link #EXECUTED}
	 */
	public boolean inExecutionPhase() {
		return this == EXECUTION || this == STOPPED || this == WARNING || this == ERROR || this == EXECUTED;
	}

	/**
	 * @return true if the state is one of {@link #STOPPED}, {@link #WARNING} or {@link #ERROR}
	 */
	public boolean inExecutionWarningPhase() {
		return this == STOPPED || this == WARNING || this == ERROR;
	}

	/**
	 * @return true if the state is {@link #CLOSED}
	 */
	public boolean inClosedPhase() {
		return this == CLOSED;
	}

	/**
	 * @return true if the state is {@link #CLOSED}
	 */
	public boolean isClosed() {
		return this == CLOSED;
	}

	/**
	 * @return true if the state is {@link #CLOSED}
	 */
	public boolean isExecuted() {
		return this == EXECUTED;
	}

	/**
	 * @return true if {@link #inExecutionPhase()} but not executed and not already in warning
	 */
	public boolean canSetToWarning() {
		return inExecutionPhase() && this != State.EXECUTED && this != State.WARNING;
	}

	/**
	 * @return true if {@link #inExecutionPhase()} but not executed and not already stopped
	 */
	public boolean canSetToStopped() {
		return inExecutionPhase() && this != State.EXECUTED && this != State.STOPPED;
	}

	/**
	 * @return true if {@link #inExecutionPhase()} but not executed and not already in error
	 */
	public boolean canSetToError() {
		return inExecutionPhase() && this != State.EXECUTED && this != State.ERROR;
	}

	/**
	 * @return true if {@link #inExecutionPhase()} but not executed
	 */
	public boolean canSetToExecuted() {
		return inExecutionPhase() && this != State.EXECUTED;
	}

	public static State parse(String s) {
		DBC.PRE.assertNotEmpty("Value may not be null", s);
		for (State state : values()) {
			if (state.state.toLowerCase().equals(s.toLowerCase()))
				return state;
		}

		throw new StrolchException("No State for " + s);
	}

	public static State max(State state1, State state2) {
		return state1.ordinal() >= state2.ordinal() ? state1 : state2;
	}

	public static State min(State state1, State state2) {
		return state1.ordinal() <= state2.ordinal() ? state1 : state2;
	}

	public static State getState(Activity activity) {

		Iterator<Entry<String, IActivityElement>> elementIterator = activity.elementIterator();

		IActivityElement first = elementIterator.next().getValue();
		State state = first.getState();

		while (elementIterator.hasNext()) {
			IActivityElement child = elementIterator.next().getValue();
			State childState = child.getState();

			// error trumps all
			if (childState == State.ERROR) {
				state = State.ERROR;
				break;
			}

			// then in execution warning
			if (childState.inExecutionWarningPhase()) {
				if (state.inExecutionWarningPhase())
					state = State.max(state, childState);
				else
					state = childState;
			}

			// then execution
			else if (childState.inExecutionPhase()) {
				if (!state.inExecutionWarningPhase()) {
					if (state.inExecutionPhase())
						state = State.min(state, childState);
					else
						state = State.EXECUTION;
				}
			}

			// then planning
			else if (childState.inPlanningPhase()) {
				if (state.inExecutionPhase()) {
					if (!state.inExecutionWarningPhase())
						state = State.EXECUTION;
				} else {
					if (state.inPlanningPhase())
						state = State.min(state, childState);
					else if ((state.inClosedPhase() || state.inCreatedPhase()) && childState.inPlanningPhase())
						state = State.PLANNING;
				}
			}

			// then created
			else if (childState.inCreatedPhase()) {
				if (state.inExecutionPhase()) {
					if (!state.inExecutionWarningPhase())
						state = State.EXECUTION;
				} else {
					if (state.inPlanningPhase()) {
						state = State.PLANNING;
					}
				}
			}

			// then closed 
			else if (childState.inClosedPhase()) {
				state = State.min(state, childState);
			}

			// should never occur
			else {
				logger.warn("Else case for getState() child: " + child.getLocator() + " childState: " + childState
						+ " state: " + state);
			}
		}

		return state;
	}
}
