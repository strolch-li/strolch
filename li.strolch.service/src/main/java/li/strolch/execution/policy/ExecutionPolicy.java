package li.strolch.execution.policy;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.StrolchRealm;
import li.strolch.command.UpdateActivityCommand;
import li.strolch.exception.StrolchException;
import li.strolch.execution.DelayedExecutionTimer;
import li.strolch.execution.ExecutionHandler;
import li.strolch.model.State;
import li.strolch.model.activity.Action;
import li.strolch.model.activity.Activity;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.policy.StrolchPolicy;
import li.strolch.privilege.base.PrivilegeException;
import li.strolch.privilege.model.Certificate;
import li.strolch.privilege.model.PrivilegeContext;
import li.strolch.runtime.StrolchConstants;
import li.strolch.runtime.privilege.PrivilegedRunnable;

/**
 * <p>
 * The {@link ExecutionPolicy} is used to execute {@link Activity Activities} and {@link Action Actions}. Execution is
 * performed by calling {@link #toExecution(Action)} and the execution policy then implements some logic to change the
 * state to {@link State#EXECUTION}. Here often communication with an external system is performed.
 * </p>
 * 
 * <p>
 * Note that the public methods on this interface are always called from a instance of the concrete class, thus any
 * instance fields are not kept from previous invocations
 * </p>
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public abstract class ExecutionPolicy extends StrolchPolicy {

	/**
	 * The TX for this execution policy. The TX needs to be updated when this execution policy has a longer life time
	 * than the actual TX
	 */
	private StrolchTransaction tx;

	public ExecutionPolicy(ComponentContainer container, StrolchTransaction tx) {
		super(container, tx);
		this.tx = tx;
	}

	/**
	 * Returns the TX which is defined on this class, not the one defined on {@link StrolchPolicy}
	 */
	@Override
	protected StrolchTransaction tx() {
		if (!this.tx.isOpen() && !this.tx.isCommitting())
			throw new IllegalStateException(
					"The TX is in state " + this.tx.getState() + " and can not be used anymore!");
		return this.tx;
	}

	/**
	 * Starts the execution of the given {@link Action}, i.e. sets the state to {@link State#EXECUTION}
	 * 
	 * @param action
	 *            the action to start execution for
	 */
	public abstract void toExecution(Action action);

	/**
	 * Completes execution of the given {@link Action}, i.e. sets the state to {@link State#EXECUTED}
	 * 
	 * @param action
	 *            the action to set to executed
	 */
	public abstract void toExecuted(Action action);

	/**
	 * Stops the execution of this {@link Action} without completing its execution, i.e. sets the state to
	 * {@link State#STOPPED}
	 * 
	 * @param action
	 *            the action to stop execution for
	 */
	public abstract void toStopped(Action action);

	/**
	 * Sets this {@link Action} which should be in execution to an error state, i.e. sets the state to
	 * {@link State#ERROR}
	 * 
	 * @param action
	 *            the action to set to error state
	 */
	public abstract void toError(Action action);

	/**
	 * Sets this {@link Action} which should be in execution to a warning state, i.e. sets the state to
	 * {@link State#WARNING}
	 * 
	 * @param action
	 *            the action to set to warning state
	 */
	public void toWarning(Action action) {

		action.setState(State.WARNING);

		UpdateActivityCommand command = new UpdateActivityCommand(getContainer(), tx());
		command.setActivity(action.getRootElement());
		command.doCommand();

		logger.warn("Action " + action.getLocator() + " is now in WARNING!");
	}

	/**
	 * @return the {@link DelayedExecutionTimer} to simplify the delayed execution of an {@link Action}, e.g. for
	 *         simulated execution or simple wait tasks
	 */
	protected DelayedExecutionTimer getDelayedExecutionTimer() {
		return getComponent(ExecutionHandler.class).getDelayedExecutionTimer();
	}

	/**
	 * Returns the execution handler instance
	 * 
	 * @return the {@link ExecutionHandler} instance
	 */
	protected ExecutionHandler getExecutionHandler() {
		return getComponent(ExecutionHandler.class);
	}

	/**
	 * Opens a {@link StrolchTransaction} where the realm retrieved using
	 * {@link ComponentContainer#getRealm(Certificate)}. This transaction should be used in a try-with-resource clause
	 * so it is properly closed.
	 * 
	 * @param action
	 *            the action to use for the opened TX
	 * 
	 * @return the open {@link StrolchTransaction}
	 * 
	 * @throws StrolchException
	 *             if the {@link StrolchRealm} does not exist with the given name
	 */
	protected StrolchTransaction openTx(PrivilegeContext ctx) throws StrolchException {
		if (this.tx.isOpen())
			throw new IllegalStateException("The current TX is still open, so can't open another one!");

		this.tx = getContainer().getRealm(ctx.getCertificate()).openTx(ctx.getCertificate(), getClass());
		return this.tx;
	}

	/**
	 * Performs the given {@link PrivilegedRunnable} as the system user {@link StrolchConstants#SYSTEM_USER_EXECUTION}
	 * 
	 * @param runnable
	 *            the runnable to perform
	 * 
	 * @throws PrivilegeException
	 */
	protected void runAsAgent(PrivilegedRunnable runnable) throws PrivilegeException {
		getContainer().getPrivilegeHandler().runAs(StrolchConstants.SYSTEM_USER_AGENT, runnable);
	}

}
