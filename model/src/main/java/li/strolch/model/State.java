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

import static java.util.stream.Collectors.joining;
import static java.util.stream.Collectors.toSet;

import java.util.Set;

import li.strolch.exception.StrolchException;
import li.strolch.model.activity.Activity;
import li.strolch.utils.dbc.DBC;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public enum State {

	CREATED("Created"),
	PLANNING("Planning"),
	PLANNED("Planned"),
	EXECUTABLE("Executable"),
	EXECUTION("Execution"),
	WARNING("Warning"),
	ERROR("Error"),
	STOPPED("Stopped"),
	EXECUTED("Executed"),
	CLOSED("Closed");

	private final String state;

	State(String state) {
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
	 * @return true if {@link #inCreatedPhase()} or {@link #inPlanningPhase()} returns true
	 */
	public boolean inCreatedPlanningPhase() {
		return inCreatedPhase() || inPlanningPhase();
	}

	/**
	 * @return true if the state is one of {@link #EXECUTION}, {@link #STOPPED}, {@link #WARNING}, {@link #ERROR} or
	 * {@link #EXECUTED}
	 */
	public boolean inExecutionPhase() {
		return this == EXECUTION || this == STOPPED || this == WARNING || this == ERROR;
	}

	/**
	 * @return true if {@link #inPlanningPhase()} or {@link #inExecutionPhase()} returns true
	 */
	public boolean inExecutionPlanningPhase() {
		return inPlanningPhase() || inExecutionPhase();
	}

	/**
	 * @return true if the state is {@link #ERROR} or {@link #STOPPED}
	 */
	public boolean inErrorPhase() {
		return this == ERROR || this == STOPPED;
	}

	/**
	 * @return true if the state is {@link #EXECUTED}
	 */
	public boolean inExecutedPhase() {
		return this == EXECUTED;
	}

	/**
	 * @return true if the state is {@link #EXECUTED} or {@link #CLOSED}
	 */
	public boolean inClosedPhase() {
		return this == EXECUTED || this == CLOSED;
	}

	/**
	 * @return true if the state is {@link #CREATED}
	 */
	public boolean isCreated() {
		return this == CREATED;
	}

	/**
	 * @return true if the state is {@link #PLANNING}
	 */
	public boolean isInPlanning() {
		return this == PLANNING;
	}

	/**
	 * @return true if the state is {@link #PLANNED}
	 */
	public boolean isPlanned() {
		return this == PLANNED;
	}

	/**
	 * @return true if the state is {@link #EXECUTION} or {@link #WARNING}
	 */
	public boolean isInExecution() {
		return this == EXECUTION || this == WARNING;
	}

	/**
	 * @return true if the state is {@link #WARNING}
	 */
	public boolean isInWarning() {
		return this == WARNING;
	}

	/**
	 * @return true if the state is {@link #ERROR}
	 */
	public boolean isInError() {
		return this == ERROR;
	}

	/**
	 * @return true if the state is {@link #STOPPED}
	 */
	public boolean isStopped() {
		return this == STOPPED;
	}

	/**
	 * @return true if the state is {@link #EXECUTED}
	 */
	public boolean isExecuted() {
		return this == EXECUTED || this == CLOSED;
	}

	/**
	 * @return true if the state is {@link #CLOSED}
	 */
	public boolean isClosed() {
		return this == CLOSED;
	}

	/**
	 * @return true if {@link #CREATED} or {@link #PLANNING} or {@link #PLANNED} or {@link #STOPPED}
	 */
	public boolean canSetToCreated() {
		return this == CREATED || this == PLANNING || this == PLANNED || this == STOPPED;
	}

	/**
	 * @return true if {@link #CREATED} or {@link #PLANNING}
	 */
	public boolean canSetToPlanning() {
		return this == CREATED || this == PLANNING;
	}

	/**
	 * @return true if {@link #CREATED} or {@link #PLANNING} or {@link #PLANNED}
	 */
	public boolean canSetToPlanned() {
		return this == CREATED || this == PLANNING || this == PLANNED;
	}

	/**
	 * @return true if {@link #PLANNED} or {@link #STOPPED}
	 */
	public boolean canSetToExecution() {
		return this == PLANNED || this == STOPPED;
	}

	/**
	 * @return true if {@link #EXECUTION} or {@link #WARNING}
	 */
	public boolean canSetToWarning() {
		return this == EXECUTION || this == WARNING;
	}

	/**
	 * @return true if {@link #EXECUTION} or {@link #ERROR} or {@link #STOPPED} or {@link #WARNING}
	 */
	public boolean canSetToStopped() {
		return this == State.EXECUTION || this == ERROR || this == State.STOPPED || this == State.WARNING;
	}

	/**
	 * @return true if {@link #PLANNED} or {@link #EXECUTION} or {@link #WARNING} or {@link #ERROR}
	 */
	public boolean canSetToError() {
		return this == PLANNED || this == EXECUTION || this == WARNING || this == ERROR;
	}

	/**
	 * @return true if {@link #EXECUTION} or {@link #WARNING} or {@link #STOPPED} or {@link #EXECUTED}
	 */
	public boolean canSetToExecuted() {
		return this == EXECUTION || this == WARNING || this == STOPPED || this == EXECUTED;
	}

	/**
	 * @return true if {@link #EXECUTED}
	 */
	public boolean canSetToClosed() {
		return this == EXECUTED;
	}

	public static State parseAllowNull(String s) {
		for (State state : values()) {
			if (state.state.equalsIgnoreCase(s))
				return state;
		}

		return null;
	}

	public static State parse(String s) {
		DBC.PRE.assertNotEmpty("Value may not be null", s);
		for (State state : values()) {
			if (state.state.equalsIgnoreCase(s))
				return state;
		}

		throw new StrolchException("No State for " + s);
	}

	public static State getState(Activity activity) {
		Set<State> states = activity.elementStream().map(e -> e.getValue().getState()).collect(toSet());
		return getState(states);
	}

	public static State getState(Set<State> states) {

		// if no states
		if (states.isEmpty())
			return CREATED;

		// if only one state
		if (states.size() == 1)
			return states.iterator().next();

		// error
		if (states.contains(ERROR))
			return ERROR;

		// stopped
		if (states.contains(STOPPED))
			return STOPPED;

		// warning
		if (states.contains(WARNING))
			return WARNING;

		// execution
		if (states.contains(EXECUTABLE) || states.contains(EXECUTION))
			return EXECUTION;
		if (states.contains(EXECUTED) && (states.contains(CREATED) || states.contains(PLANNING) || states.contains(
				PLANNED)))
			return EXECUTION;

		// executed
		if (states.contains(EXECUTED) && (states.contains(CLOSED)))
			return EXECUTED;

		// planning
		if (states.contains(PLANNING))
			return PLANNING;
		if (states.contains(PLANNED) && (states.contains(CREATED) || states.contains(PLANNING)))
			return PLANNING;

		// planned
		if (states.contains(PLANNED) && (states.contains(CLOSED)))
			return PLANNED;

		if (states.contains(CREATED) && (states.contains(CLOSED)))
			return CREATED;

		// should never happen, unless new state is introduced
		throw new IllegalStateException(
				"Unhandled situation with states: " + states.stream().map(e -> e.state).collect(joining(", ")));
	}
}
