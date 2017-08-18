package li.strolch.execution.policy;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.exception.StrolchModelException;
import li.strolch.model.Locator;
import li.strolch.model.Resource;
import li.strolch.model.State;
import li.strolch.model.activity.Action;
import li.strolch.model.parameter.BooleanParameter;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.persistence.api.UpdateResourceCommand;

/**
 * <p>
 * This extension of the {@link DurationExecution} overrides the {@link #isExecutable(Action)} method and validates that
 * the {@link Resource} to which the {@link Action} is attached, has a {@link BooleanParameter} <code>reserved</code>
 * and only allows execution if the value is false, in which case the {@link #toExecution(Action)} method sets the value
 * to true, and the {@link #toExecuted(Action)} method returns the value to false.
 * </p>
 * 
 * <p>
 * <b>Note:</b> the reservation is done for {@link Action} of type {@link #TYPE_RESERVE} and releasing is done for
 * {@link Action} of type {@link #TYPE_RELEASE}
 * </p>
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class ReservationExection extends DurationExecution {

	public static final String PARAM_RESERVED = "reserved";
	public static final String BAG_PARAMETERS = "parameters";
	public static final String TYPE_RESERVE = "Reserve";
	public static final String TYPE_RELEASE = "Release";

	public ReservationExection(ComponentContainer container, StrolchTransaction tx) {
		super(container, tx);
	}

	@Override
	public boolean isExecutable(Action action) {

		// only check if reserve
		if (!action.getType().equals(TYPE_RESERVE) && !action.getType().equals(TYPE_RELEASE)) {
			// otherwise delegate to super class
			return super.isExecutable(action);
		}

		if (action.getType().equals(TYPE_RESERVE))
			return !isReserved(action);
		else
			return isReserved(action);
	}

	protected boolean isReserved(Action action) {
		// get resource
		Resource resource = getResource(action);

		if (!resource.hasParameter(BAG_PARAMETERS, PARAM_RESERVED))
			throw new StrolchModelException("Parameter " + PARAM_RESERVED + " on bag " + BAG_PARAMETERS + " missing on "
					+ resource.getLocator());

		BooleanParameter reservedP = resource.getParameter(BAG_PARAMETERS, PARAM_RESERVED);
		return reservedP.getValue().booleanValue();
	}

	@Override
	public void toExecution(Action action) {

		// only do if reserve
		if (!action.getType().equals(TYPE_RESERVE) && !action.getType().equals(TYPE_RELEASE)) {
			// otherwise delegate to super class
			super.toExecution(action);

			return;
		}

		setReservation(action);

		String realmName = tx().getRealmName();
		Locator locator = action.getLocator();
		getDelayedExecutionTimer().execute(realmName, getContainer(), locator, 0L);

		setActionState(action, State.EXECUTION);
	}

	@Override
	public void toExecuted(Action action) {

		// only do if release
		if (!action.getType().equals(TYPE_RESERVE) && !action.getType().equals(TYPE_RELEASE)) {
			// otherwise delegate to super class
			super.toExecuted(action);

			return;
		}

		setReservation(action);
		setActionState(action, State.EXECUTED);
	}

	public void setReservation(Action action) {

		Resource resource = getResource(action);

		boolean reserved = action.getType().equals(TYPE_RESERVE);

		// release the resource
		BooleanParameter reservedP = resource.getParameter(BAG_PARAMETERS, PARAM_RESERVED);
		reservedP.setValue(reserved);

		// save changes
		UpdateResourceCommand command = new UpdateResourceCommand(getContainer(), tx());
		command.setResource(resource);
		command.doCommand();
	}
}
