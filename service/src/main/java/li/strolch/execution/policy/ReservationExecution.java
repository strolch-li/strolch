package li.strolch.execution.policy;

import static li.strolch.model.StrolchModelConstants.BAG_PARAMETERS;
import static li.strolch.model.StrolchModelConstants.PolicyConstants.*;
import static li.strolch.utils.ObjectHelper.isIn;

import li.strolch.exception.StrolchModelException;
import li.strolch.execution.Controller;
import li.strolch.model.Resource;
import li.strolch.model.StrolchModelConstants;
import li.strolch.model.activity.Action;
import li.strolch.model.activity.Activity;
import li.strolch.model.parameter.BooleanParameter;
import li.strolch.model.parameter.IntegerParameter;
import li.strolch.model.parameter.StringListParameter;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.runtime.StrolchConstants;

/**
 * <p>This reservation execution policy has two modes of functioning:</p>
 * <ul>
 *     <li>Reserving the executing {@link Action}'s {@link Resource} using the {@link BooleanParameter} with ID
 *     {@link StrolchModelConstants.PolicyConstants#PARAM_RESERVED} </li>
 *     <li>Validating that no more than a certain number of jobs are running using the
 *     {@link StrolchModelConstants.PolicyConstants#PARAM_JOB_COUNT_SEMAPHORE} as the max number of jobs. Which jobs
 *     are considered is either the {@link Action#getRootElement()}'s type, or the list of types defined on the
 *     {@link StringListParameter} with ID {@link StrolchModelConstants.PolicyConstants#PARAM_JOB_COUNT_SEMAPHORE_TYPES}
 *     which is found using {@link Action#findParameter(String, String)}</li>
 * </ul>
 *
 * <p>
 * The rules are enforced in the {@link #isExecutable(Action)} method, and simply executed in the {@link #toExecuted(Action)} method
 * </p>
 *
 * <p>
 * <b>Note 1:</b> the reservation is done for {@link Action} of type {@link StrolchConstants.PolicyConstants#TYPE_RESERVE}
 * and releasing is done for {@link Action} of type {@link StrolchConstants.PolicyConstants#TYPE_RELEASE}
 * </p>
 *
 * <p><b>Note 2:</b> the semaphore is done of {@link Action Actions} of type {@link StrolchModelConstants.PolicyConstants#TYPE_JOB_COUNT_SEMAPHORE}</p>
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class ReservationExecution extends DurationExecution {

	public ReservationExecution(StrolchTransaction tx) {
		super(tx);
	}

	@Override
	public boolean isExecutable(Action action) {
		return switch (action.getType()) {
			case TYPE_RESERVE -> !isReserved(tx(), action);
			case TYPE_RELEASE -> true;
			case TYPE_JOB_COUNT_SEMAPHORE -> jobCountSemaphoreSatisfied(action);

			default -> super.isExecutable(action);
		};
	}

	@Override
	public void toExecution(Action action) {
		switch (action.getType()) {
		case TYPE_RESERVE, TYPE_RELEASE, TYPE_JOB_COUNT_SEMAPHORE -> toExecuted(action);
		default -> super.toExecution(action);
		}
	}

	@Override
	public void toExecuted(Action action) {
		switch (action.getType()) {
		case TYPE_RESERVE -> setReservation(tx(), action, true);
		case TYPE_RELEASE -> setReservation(tx(), action, false);
		default -> {
			// do nothing
		}
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

	protected boolean jobCountSemaphoreSatisfied(Action action) {
		StringListParameter jobCountSemaphoreTypesP = action.findObjectivesParam(PARAM_JOB_COUNT_SEMAPHORE_TYPES,
				false);
		String[] types = jobCountSemaphoreTypesP == null ?
				new String[] { action.getRootElement().getType() } :
				jobCountSemaphoreTypesP.getValue().toArray(String[]::new);

		long nrOfActivitiesInExecution = getExecutionHandler().getControllers(tx().getRealmName())
				.stream()
				.map(Controller::getActivity)
				.filter(a -> isIn(a.getType(), types, false))
				.filter(Activity::inExecutionPhase)
				.count();

		IntegerParameter jobCountSemaphoreP = action.findObjectivesParam(PARAM_JOB_COUNT_SEMAPHORE, true);
		if (nrOfActivitiesInExecution < jobCountSemaphoreP.getValue())
			return true;

		logger.error("Action " + action.getLocator() + " is not executable as there are " + nrOfActivitiesInExecution
				+ " activities in execution with type(s): " + jobCountSemaphoreP.getValueAsString());
		return false;
	}
}
