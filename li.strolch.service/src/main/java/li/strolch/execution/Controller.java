package li.strolch.execution;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import li.strolch.execution.command.ExecuteActivityCommand;
import li.strolch.execution.policy.ExecutionPolicy;
import li.strolch.model.Locator;
import li.strolch.model.State;
import li.strolch.model.activity.Activity;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.base.PrivilegeException;
import li.strolch.privilege.model.Certificate;
import li.strolch.privilege.model.PrivilegeContext;
import li.strolch.runtime.privilege.PrivilegedRunnable;

public class Controller {

	private ExecutionHandler executionHandler;
	private Activity activity;
	private Map<Locator, ExecutionPolicy> inExecution;

	public Controller(ExecutionHandler executionHandler, Activity activity) {
		this.executionHandler = executionHandler;
		this.activity = activity;
		this.inExecution = new HashMap<>();
	}

	public State getState() {
		return this.activity.getState();
	}

	public Activity getActivity() {
		return this.activity;
	}

	public Set<Locator> getInExecution() {
		return this.inExecution.keySet();
	}

	protected StrolchTransaction openTx(Certificate cert) {
		return this.executionHandler.openTx(cert, getClass(), false);
	}

	protected void runAsAgent(PrivilegedRunnable runnable) throws PrivilegeException, Exception {
		this.executionHandler.runAsAgent(runnable);
	}

	private Activity refreshActivity(StrolchTransaction tx) {
		this.activity = tx.getActivityBy(this.activity.getType(), this.activity.getId(), true);
		return this.activity;
	}

	public void execute() {
		try (StrolchTransaction tx = openTx(ctx.getCertificate())) {

			ExecuteActivityCommand command = new ExecuteActivityCommand(tx);
			command.setActivity(refreshActivity(tx));
			command.validate();
			command.doCommand();

			tx.commitOnClose();
		}
	}

	public void stop() {

	}
}
