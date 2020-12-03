package li.strolch.execution.policy;

import static li.strolch.runtime.StrolchConstants.SYSTEM_USER_AGENT;
import static li.strolch.utils.helper.StringHelper.formatMillisecondsDuration;

import java.time.Duration;
import java.util.concurrent.ThreadLocalRandom;
import java.util.concurrent.TimeUnit;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.StrolchRealm;
import li.strolch.exception.StrolchException;
import li.strolch.execution.Controller;
import li.strolch.execution.DelayedExecutionTimer;
import li.strolch.execution.ExecutionHandler;
import li.strolch.model.Locator;
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
import li.strolch.runtime.privilege.PrivilegedRunnableWithResult;

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

	private Controller controller;
	private boolean stopped;

	protected String realm;
	protected String actionType;
	protected Locator actionLoc;

	/**
	 * The TX for this execution policy. The TX needs to be updated when this execution policy has a longer life time
	 * than the actual TX
	 */
	private StrolchTransaction tx;

	public ExecutionPolicy(StrolchTransaction tx) {
		super(tx);
		this.tx = tx;
		this.realm = tx.getRealmName();
	}

	public void setController(StrolchTransaction tx, Controller controller) {
		this.tx = tx;
		this.controller = controller;
	}

	public Controller getController() {
		return this.controller;
	}

	public boolean isStopped() {
		return this.stopped;
	}

	public void setStopped(boolean stopped) {
		this.stopped = stopped;
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
	 * <p>
	 * Evaluates if the given {@link Action} is executable i.e. any state has the expected values so that the given
	 * {@link Action} can have its execution be started.
	 * </p>
	 *
	 * <p>
	 * <b>Note:</b> The default implementation is to always allow execution. Subclasses can override this method and
	 * define a required state which is to be met for execution of the given {@link Action}.
	 * </p>
	 *
	 * @param action
	 * 		the {@link Action} to check if it can be executed
	 *
	 * @return true if the action can be executed, false if not, i.e. the current state disallows the action to be
	 * executed
	 */
	public boolean isExecutable(Action action) {
		return true;
	}

	/**
	 * Performs any initialization of this {@link ExecutionPolicy} for the given action, this method stores the {@link
	 * Locator} of the action and its type
	 *
	 * @param action
	 * 		the action for which to initialize
	 */
	public void initialize(Action action) {
		this.actionType = action.getType();
		this.actionLoc = action.getLocator();
	}

	/**
	 * Starts the execution of the given {@link Action}, i.e. sets the state to {@link State#EXECUTION}
	 *
	 * @param action
	 * 		the action to start execution for
	 */
	public abstract void toExecution(Action action);

	/**
	 * Completes execution of the given {@link Action}, i.e. sets the state to {@link State#EXECUTED}
	 *
	 * @param action
	 * 		the action to set to executed
	 */
	public abstract void toExecuted(Action action);

	/**
	 * Stops the execution of this {@link Action} without completing its execution, i.e. sets the state to {@link
	 * State#STOPPED}
	 *
	 * @param action
	 * 		the action to stop execution for
	 */
	public abstract void toStopped(Action action);

	/**
	 * Sets this {@link Action} which should be in execution to an error state, i.e. sets the state to {@link
	 * State#ERROR}
	 *
	 * @param action
	 * 		the action to set to error state
	 */
	public abstract void toError(Action action);

	/**
	 * Sets this {@link Action} which should be in execution to a warning state, i.e. sets the state to {@link
	 * State#WARNING}
	 *
	 * @param action
	 * 		the action to set to warning state
	 */
	public abstract void toWarning(Action action);

	/**
	 * Stops any active components in this {@link ExecutionPolicy}. This is used to notify the {@link ExecutionPolicy}
	 * that any scheduled actions should be stopped, that listening and observing registrations can be taken back, etc.
	 */
	public void stop() {
		this.stopped = true;
		try {
			handleStopped();
		} catch (Exception e) {
			logger.error("Stopping failed for " + this.actionLoc, e);
		}
	}

	protected void handleStopped() {
		getDelayedExecutionTimer().cancel(this.actionLoc);
	}

	protected void setActionState(Action action, State state) {

		action.setState(state);

		tx().update(action.getRootElement());

		String msg = "Action " + action.getLocator() + " is now in state " + state;
		if (state == State.ERROR)
			logger.error(msg);
		else if (state == State.STOPPED)
			logger.warn(msg);
		else
			logger.info(msg);
	}

	/**
	 * Delays the given {@link Runnable} by the given {@link Duration}
	 *
	 * @param duration
	 * 		the duration to delay
	 * @param runnable
	 * 		the action to delay
	 */
	public void delay(Duration duration, Runnable runnable) {
		long delayMs = duration.toMillis();
		getDelayedExecutionTimer().delay(delayMs, runnable);
	}

	/**
	 * Method to delay toExecuted() call for this action by the given duration
	 *
	 * @param duration
	 * 		the delay duration
	 */
	protected void delayToExecutedBy(Duration duration) {
		long delayMs = duration.toMillis();
		if (delayMs < 20) {
			logger.warn("Delay time for " + this.actionLoc + " is less than 20ms, overriding!");
			delayMs = 20;
		}
		logger.info("Delaying toExecuted of " + this.actionLoc + " by " + formatMillisecondsDuration(delayMs));
		getDelayedExecutionTimer().execute(this.realm, getContainer(), this.actionLoc, delayMs);
	}

	protected void delayToExecutedByRandom(long duration, double minFactor, double maxFactor, TimeUnit delayUnit) {
		delayToExecutedByRandom((long) (duration * minFactor), (long) (duration * maxFactor), delayUnit);
	}

	protected void delayToExecutedByRandom(long min, long max, TimeUnit delayUnit) {
		long delay = ThreadLocalRandom.current().nextLong(min, max + 1);
		delayToExecutedBy(delay, delayUnit);
	}

	/**
	 * Method to delay toExecuted() call for this action by the given amount
	 *
	 * @param delay
	 * 		the delay time
	 * @param delayUnit
	 * 		the UOM of the delay time
	 */
	protected void delayToExecutedBy(long delay, TimeUnit delayUnit) {
		long delayMs = delayUnit.toMillis(delay);
		if (delayMs < 20) {
			logger.warn("Delay time for " + this.actionLoc + " is less than 20ms, overriding!");
			delayMs = 20;
		}
		logger.info("Delaying toExecuted of " + this.actionLoc + " by " + formatMillisecondsDuration(delayMs));
		getDelayedExecutionTimer().execute(this.realm, getContainer(), this.actionLoc, delayMs);
	}

	/**
	 * @return the {@link DelayedExecutionTimer} to simplify the delayed execution of an {@link Action}, e.g. for
	 * simulated execution or simple wait tasks
	 */
	private DelayedExecutionTimer getDelayedExecutionTimer() {
		return getComponent(ExecutionHandler.class).getDelayedExecutionTimer();
	}

	/**
	 * Opens a {@link StrolchTransaction} where the realm retrieved using {@link ComponentContainer#getRealm(Certificate)}.
	 * This transaction should be used in a try-with-resource clause so it is properly closed.
	 *
	 * @param ctx
	 * 		the privilege context
	 * @param readOnly
	 * 		true if this is a read-only TX
	 *
	 * @return the open {@link StrolchTransaction}
	 *
	 * @throws StrolchException
	 * 		if the {@link StrolchRealm} does not exist with the given name
	 */
	protected StrolchTransaction openTx(PrivilegeContext ctx, boolean readOnly) throws StrolchException {
		if (this.tx.isOpen())
			throw new IllegalStateException("The current TX is still open, so can't open another one!");

		this.tx = getContainer().getRealm(ctx.getCertificate()).openTx(ctx.getCertificate(), getClass(), readOnly);
		return this.tx;
	}

	/**
	 * Performs the given {@link PrivilegedRunnable} as the system user {@link StrolchConstants#SYSTEM_USER_AGENT}
	 *
	 * @param runnable
	 * 		the runnable to perform
	 *
	 * @throws PrivilegeException
	 * 		if the agent is missing the privilege
	 * @throws Exception
	 * 		if anything else goes wrong during execution
	 */
	protected void runAsAgent(PrivilegedRunnable runnable) throws PrivilegeException, Exception {
		getContainer().getPrivilegeHandler().runAs(SYSTEM_USER_AGENT, runnable);
	}

	/**
	 * Performs the given {@link PrivilegedRunnableWithResult} as the system user {@link
	 * StrolchConstants#SYSTEM_USER_AGENT}
	 *
	 * @param runnable
	 * 		the runnable to perform
	 *
	 * @return the result of the operation
	 *
	 * @throws PrivilegeException
	 * 		if the agent is missing the privilege
	 * @throws Exception
	 * 		if anything else goes wrong during execution
	 */
	protected <T> T runAsAgentWithResult(PrivilegedRunnableWithResult<T> runnable)
			throws PrivilegeException, Exception {
		return getContainer().getPrivilegeHandler().runWithResult(SYSTEM_USER_AGENT, runnable);
	}
}
