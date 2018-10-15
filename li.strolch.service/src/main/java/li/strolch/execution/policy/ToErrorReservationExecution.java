package li.strolch.execution.policy;

import java.util.ResourceBundle;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.handler.operationslog.LogMessage;
import li.strolch.handler.operationslog.LogSeverity;
import li.strolch.model.State;
import li.strolch.model.activity.Action;
import li.strolch.persistence.api.StrolchTransaction;

/**
 * The {@link ToErrorReservationExecution} executes same as {@link ReservationExection} with the difference that {@link
 * #isExecutable(Action)} always returns true, and if the action's resource is currently reserved, the execution fails
 * and the state is set to ERROR
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class ToErrorReservationExecution extends ReservationExection {

	public ToErrorReservationExecution(ComponentContainer container, StrolchTransaction tx) {
		super(container, tx);
	}

	/**
	 * In {@link ToErrorReservationExecution} we are always executable, but go to error, if already reserved
	 */
	@Override
	public boolean isExecutable(Action action) {

		tx().lock(getResource(action));

		if (action.getType().equals(TYPE_RESERVE)) {
			return true;
		}

		return super.isExecutable(action);
	}

	@Override
	public void toExecution(Action action) {

		tx().lock(getResource(action));

		if (action.getType().equals(TYPE_RESERVE) && isReserved(action)) {
			setActionState(action, State.EXECUTION);
			toError(new LogMessage(tx().getRealmName(), tx().getCertificate().getUsername(), action.getLocator(),
					LogSeverity.Error, ResourceBundle.getBundle("strolch-service"),
					"execution.policy.reservation.alreadyReserved")
					.value("resourceLoc", getResource(action).getLocator().toString()));
		} else {
			super.toExecution(action);
		}
	}
}
