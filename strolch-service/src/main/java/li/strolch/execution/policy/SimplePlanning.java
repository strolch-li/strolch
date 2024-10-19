package li.strolch.execution.policy;

import java.util.List;

import li.strolch.model.Resource;
import li.strolch.model.State;
import li.strolch.model.activity.Action;
import li.strolch.model.timedstate.StrolchTimedState;
import li.strolch.model.timevalue.IValue;
import li.strolch.model.timevalue.IValueChange;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.utils.dbc.DBC;

public class SimplePlanning extends PlanningPolicy {

	public SimplePlanning(StrolchTransaction tx) {
		super(tx);
	}

	/**
	 * Command to plan an {@link Action} to a {@link Resource}. It is assumed that the {@link IValueChange} objects of
	 * the action are already constructed. The resource is evaluated using {@link #evaluateAndSetResource(Action)}
	 *
	 * <br>
	 * <p>
	 * It iterates the {@link IValueChange} operators and registers the resulting changes on the {@link
	 * StrolchTimedState} objects assigned to the {@link Resource}.
	 *
	 * @param action
	 * 		the action to plan
	 */
	@Override
	public void plan(Action action) {
		if (action.getState().compareTo(State.PLANNED) >= 0)
			throw new IllegalStateException("Can not plan illegal state " + action.getState());

		logger.debug("Planning action {}", action.getLocator());
		action.setState(State.PLANNING);

		Resource resource = evaluateAndSetResource(action);
		if (resource == null) {
			logger.error("No resource evaluated, so can not plan {}", action.getLocator());
			tx().update(action.getRootElement());
			return;
		}

		List<IValueChange<? extends IValue<?>>> changes = action.getChanges();
		for (IValueChange<?> change : changes) {
			@SuppressWarnings("rawtypes")
			StrolchTimedState timedState = resource.getTimedState(change.getStateId());
			//noinspection unchecked
			timedState.applyChange(change, true);
		}

		action.setState(State.PLANNED);
		tx().update(action.getRootElement());
	}

	/**
	 * Command to unplan an {@link Action} from a {@link Resource}. It is assumes that the {@link Action} is already in
	 * {@link State#PLANNED} and that the {@link IValueChange} objects of the action are stored on the action.
	 *
	 * <br>
	 * <p>
	 * It iterates the {@link IValueChange} operators and unregisters the changes from the {@link StrolchTimedState}
	 * objects on the {@link Resource}.
	 *
	 * @param action
	 * 		the action to unplan
	 */
	@Override
	public void unplan(Action action) {
		DBC.PRE.assertEquals("Can not unplan illegal state", State.PLANNED, action.getState());

		Resource resource = evaluateAndSetResource(action);

		List<IValueChange<? extends IValue<?>>> changes = action.getChanges();
		for (IValueChange<?> change : changes) {
			@SuppressWarnings("rawtypes")
			StrolchTimedState timedState = resource.getTimedState(change.getStateId());
			//noinspection unchecked
			timedState.applyChange(change.getInverse(), true);
		}

		action.setState(State.CREATED);
	}
}
