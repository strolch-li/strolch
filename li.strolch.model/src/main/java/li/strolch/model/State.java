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

import java.util.Set;
import java.util.stream.Collectors;

import li.strolch.exception.StrolchException;
import li.strolch.model.activity.Activity;
import li.strolch.utils.dbc.DBC;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public enum State {

	CREATED("Created"), //$NON-NLS-1$
	PLANNING("Planning"), //$NON-NLS-1$
	PLANNED("Planned"), //$NON-NLS-1$
	STARTING("Starting"), //$NON-NLS-1$
	EXECUTION("Execution"), //$NON-NLS-1$
	WARNING("Warning"), //$NON-NLS-1$
	ERROR("Error"), //$NON-NLS-1$
	STOPPED("Stopped"), //$NON-NLS-1$
	EXECUTED("Executed"), //$NON-NLS-1$
	CLOSED("Closed"); //$NON-NLS-1$

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
		return this == EXECUTION || this == STOPPED || this == WARNING || this == ERROR;
	}

	/**
	 * @return true if the state is {@link #ERROR} or {@link #STOPPED}
	 */
	public boolean inErrorPhase() {
		return this == State.ERROR || this == State.STOPPED;
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
		return this == EXECUTED || this == CLOSED;
	}

	/**
	 * @return true if the state is {@link #CREATED}
	 */
	public boolean isCreated() {
		return this == State.CREATED;
	}

	/**
	 * @return true if the state is {@link #PLANNING}
	 */
	public boolean isInPlanning() {
		return this == State.PLANNING;
	}

	/**
	 * @return true if the state is {@link #PLANNED}
	 */
	public boolean isInPlanned() {
		return this == State.PLANNED;
	}

	/**
	 * @return true if the state is {@link #EXECUTION}
	 */
	public boolean isInExecution() {
		return this == State.EXECUTION;
	}

	/**
	 * @return true if the state is {@link #WARNING}
	 */
	public boolean isInWarning() {
		return this == State.WARNING;
	}

	/**
	 * @return true if the state is {@link #ERROR}
	 */
	public boolean isInError() {
		return this == State.ERROR;
	}

	/**
	 * @return true if the state is {@link #EXECUTED}
	 */
	public boolean isExecuted() {
		return this == EXECUTED;
	}

	/**
	 * @return true if the state is {@link #CLOSED}
	 */
	public boolean isClosed() {
		return this == CLOSED;
	}

	/**
	 * @return true if {@link #EXECUTION}
	 */
	public boolean canSetToWarning() {
		return this == EXECUTION;
	}

	/**
	 * @return true if {@link #ERROR}
	 */
	public boolean canSetToStopped() {
		return this == State.ERROR;
	}

	/**
	 * @return true if {@link #STARTING} or {@link #EXECUTION} or {@link #WARNING}
	 */
	public boolean canSetToError() {
		return this == State.STARTING || this == State.EXECUTION || this == State.WARNING;
	}

	/**
	 * @return true if {@link #EXECUTION} or {@link #WARNING} or {@link #STOPPED}
	 */
	public boolean canSetToExecuted() {
		return this == State.EXECUTION || this == State.WARNING || this == State.STOPPED;
	}

	public static State parse(String s) {
		DBC.PRE.assertNotEmpty("Value may not be null", s);
		for (State state : values()) {
			if (state.state.toLowerCase().equals(s.toLowerCase()))
				return state;
		}

		throw new StrolchException("No State for " + s);
	}

	public static State getState(Activity activity) {

		Set<State> states = activity.elementStream().map(e -> e.getValue().getState()).collect(Collectors.toSet());

		// if only one state
		if (states.size() == 1)
			return states.iterator().next();

		// error
		if (states.contains(State.ERROR))
			return State.ERROR;

		// stopped
		if (states.contains(State.STOPPED))
			return State.STOPPED;

		// warning
		if (states.contains(State.WARNING))
			return State.WARNING;

		// execution
		if (states.contains(State.EXECUTION) || states.contains(State.STARTING))
			return State.EXECUTION;
		if (states.contains(State.EXECUTED) && (states.contains(State.CREATED) || states.contains(State.PLANNING)
				|| states.contains(State.PLANNED)))
			return State.EXECUTION;

		// executed
		if (states.contains(State.EXECUTED) && (states.contains(State.CLOSED)))
			return State.EXECUTED;

		// planning
		if (states.contains(State.PLANNING))
			return State.PLANNING;
		if (states.contains(State.PLANNED) && (states.contains(State.CREATED) || states.contains(State.PLANNING)))
			return State.PLANNING;

		// planned
		if (states.contains(State.PLANNED) && (states.contains(State.CLOSED)))
			return State.PLANNED;

		// should never happen, unless new state is introduced
		throw new IllegalStateException("Unhandled situation with states: "
				+ states.stream().map(e -> e.state).collect(Collectors.joining(", ")));
	}
}
