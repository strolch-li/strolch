package li.strolch.model.activity;


/**
 * Traces the state of the {@link Action} in the sequence of creation, scheduling and planning. 
 * 
 * @author Martin Smock <smock.martin@gmail.com>
 */
public enum ActionState {
	CREATED, 
	SCHEDULED, // when the start and end time is set and the value changes have been attached
	PLANNED; // when the value changes have been registered to the states of the resources
}
