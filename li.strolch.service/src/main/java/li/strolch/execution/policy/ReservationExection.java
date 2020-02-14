package li.strolch.execution.policy;

import li.strolch.persistence.api.StrolchTransaction;

/**
 * @deprecated use {@link ReservationExecution} instead
 */
@Deprecated
public class ReservationExection extends ReservationExecution {
	public ReservationExection(StrolchTransaction tx) {
		super(tx);
	}
}
