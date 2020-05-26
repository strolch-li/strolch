package li.strolch.execution.policy;

import static li.strolch.runtime.StrolchConstants.PolicyConstants.TYPE_RESERVE;

import java.util.ResourceBundle;

import li.strolch.handler.operationslog.LogMessage;
import li.strolch.handler.operationslog.LogMessageState;
import li.strolch.handler.operationslog.LogSeverity;
import li.strolch.model.State;
import li.strolch.model.activity.Action;
import li.strolch.persistence.api.StrolchTransaction;

/**
 * The {@link ToErrorReservationExecution} executes same as {@link ReservationExecution} with the difference that {@link
 * #isExecutable(Action)} always returns true, and if the action's resource is currently reserved, the execution fails
 * and the state is set to ERROR
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class ToErrorReservationExecution extends ReservationExecution {

	public ToErrorReservationExecution(StrolchTransaction tx) {
		super(tx);
	}

	/**
	 * In {@link ToErrorReservationExecution} we are always executable, but go to error, if already reserved
	 */
	@Override
	public boolean isExecutable(Action action) {

		if (action.getType().equals(TYPE_RESERVE)) {
			return true;
		}

		return super.isExecutable(action);
	}

	@Override
	public void toExecution(Action action) {

		if (action.getType().equals(TYPE_RESERVE) && isReserved(tx(), action)) {
			setActionState(action, State.EXECUTION);
			toError(new LogMessage(tx().getRealmName(), tx().getCertificate().getUsername(), action.getLocator(),
					LogSeverity.Error, LogMessageState.Information, ResourceBundle.getBundle("strolch-service"),
					"execution.policy.reservation.alreadyReserved")
					.value("resourceLoc", action.getResourceLocator().toString()));
		} else {
			super.toExecution(action);
		}
	}
}
