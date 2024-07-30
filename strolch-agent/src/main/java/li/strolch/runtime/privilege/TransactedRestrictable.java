package li.strolch.runtime.privilege;

import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.SimpleRestrictable;
import li.strolch.utils.dbc.DBC;

public class TransactedRestrictable extends SimpleRestrictable {

	private final StrolchTransaction tx;

	public TransactedRestrictable(StrolchTransaction tx, String name, Object value) {
		super(name, value);
		DBC.PRE.assertNotNull("tx must not be null", tx);
		this.tx = tx;
	}

	public StrolchTransaction tx() {
		return this.tx;
	}
}
