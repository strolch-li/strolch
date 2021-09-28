package li.strolch.execution.policy;

import static li.strolch.model.StrolchModelConstants.BAG_PARAMETERS;
import static li.strolch.model.StrolchModelConstants.PolicyConstants.*;

import li.strolch.exception.StrolchModelException;
import li.strolch.model.Resource;
import li.strolch.model.activity.Action;
import li.strolch.model.parameter.BooleanParameter;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.runtime.StrolchConstants;

/**
 * <p>
 * This extension of the {@link DurationExecution} overrides the {@link #isExecutable(Action)} method and validates that
 * the {@link Resource} to which the {@link Action} is attached, has a {@link BooleanParameter} <code>reserved</code>
 * and only allows execution if the value is false, in which case the {@link #toExecution(Action)} method sets the value
 * to true, and the {@link #toExecuted(Action)} method returns the value to false.
 * </p>
 *
 * <p>
 * <b>Note:</b> the reservation is done for {@link Action} of type {@link StrolchConstants.PolicyConstants#TYPE_RESERVE}
 * and releasing is done for {@link Action} of type {@link StrolchConstants.PolicyConstants#TYPE_RELEASE}
 * </p>
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class ReservationExecution extends DurationExecution {

	public ReservationExecution(StrolchTransaction tx) {
		super(tx);
	}

	@Override
	public boolean isExecutable(Action action) {
		switch (action.getType()) {
		case TYPE_RESERVE:
			return !isReserved(tx(), action);
		case TYPE_RELEASE:
			return true;
		default:
			return super.isExecutable(action);
		}
	}

	@Override
	public void toExecution(Action action) {
		switch (action.getType()) {
		case TYPE_RESERVE:
		case TYPE_RELEASE:

			toExecuted(action);
			break;

		default:
			super.toExecution(action);
		}
	}

	@Override
	public void toExecuted(Action action) {
		switch (action.getType()) {

		case TYPE_RESERVE:
			setReservation(tx(), action, true);
			break;
		case TYPE_RELEASE:
			setReservation(tx(), action, false);
			break;
		}

		super.toExecuted(action);
	}

	public static boolean isReserved(StrolchTransaction tx, Action action) {

		// get resource
		Resource resource = tx.getResourceFor(action, true);

		if (!resource.hasParameter(BAG_PARAMETERS, PARAM_RESERVED))
			throw new StrolchModelException("Parameter " + PARAM_RESERVED + " on bag " + BAG_PARAMETERS + " missing on "
					+ resource.getLocator());

		BooleanParameter reservedP = resource.getParameter(BAG_PARAMETERS, PARAM_RESERVED);
		return reservedP.getValue();
	}

	public static void setReservation(StrolchTransaction tx, Action action, boolean isReserve) {
		Resource resource = tx.getResourceFor(action, true);

		// release the resource
		BooleanParameter reservedP = resource.getParameter(BAG_PARAMETERS, PARAM_RESERVED);
		reservedP.setValue(isReserve);

		// save changes
		tx.update(resource);
	}
}
