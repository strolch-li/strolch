package li.strolch.execution.policy;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.StrolchRealm;
import li.strolch.exception.StrolchException;
import li.strolch.execution.Controller;
import li.strolch.execution.DelayedExecutionTimer;
import li.strolch.execution.ExecutionHandler;
import li.strolch.model.Locator;
import li.strolch.model.Resource;
import li.strolch.model.State;
import li.strolch.model.activity.Action;
import li.strolch.model.activity.Activity;
import li.strolch.model.parameter.DurationParameter;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.policy.StrolchPolicy;
import li.strolch.privilege.base.PrivilegeException;
import li.strolch.privilege.model.Certificate;
import li.strolch.privilege.model.PrivilegeContext;
import li.strolch.runtime.StrolchConstants;
import li.strolch.runtime.privilege.PrivilegedRunnable;
import li.strolch.runtime.privilege.PrivilegedRunnableWithResult;
import li.strolch.utils.time.PeriodDuration;

import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.ThreadLocalRandom;
import java.util.concurrent.TimeUnit;

import static li.strolch.model.StrolchModelConstants.PolicyConstants.PARAM_DURATION;
import static li.strolch.runtime.StrolchConstants.SYSTEM_USER_AGENT;
import static li.strolch.utils.helper.StringHelper.formatMillisecondsDuration;

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

	protected final String realm;
	protected String actionType;
	protected Locator resourceLoc;
	protected Locator actionLoc;

	protected List<ScheduledFuture<?>> futures;

	/**
	 * The TX for this execution policy. The TX needs to be updated when this execution policy has a longer life time
	 * than the actual TX
	 */
	private StrolchTransaction tx;

	public ExecutionPolicy(StrolchTransaction tx) {
		super(tx);
		this.tx = tx;
		this.realm = tx.getRealmName();
		this.futures = new ArrayList<>();
	}

	/**
	 * <p>Set the {@link Controller} for this execution policy. Usually called by the controller itself, when
	 * instantiating this instance</p>
	 *
	 * <p><b>Note:</b> This is used as execution policies can have a longer lifecycle than its transaction.</p>
	 *
	 * @param tx         the update TX
	 * @param controller the controller
	 */
	public void refreshController(StrolchTransaction tx, Controller controller) {
		this.tx = tx;
		this.controller = controller;
		this.stopped = false;
	}

	/**
	 * Returns the controller, controlling this execution policy
	 *
	 * @return the controller, controlling this execution policy
	 */
	public Controller getController() {
		return this.controller;
	}

	/**
	 * Returns the type of activity for which this {@link ExecutionPolicy} is being executed
	 */
	public String getActivityType() {
		return this.actionLoc.get(1);
	}

	/**
	 * Returns the type of the {@link Resource} for this action. Can only be called after {@link #initialize(Action)}
	 * was called.
	 */
	protected String getResourceType() {
		return this.resourceLoc.get(1);
	}

	/**
	 * Returns the id of the {@link Resource} for this action. Can only be called after {@link #initialize(Action)} was
	 * called.
	 */
	protected String getResourceId() {
		return this.resourceLoc.get(2);
	}

	/**
	 * Returns the {@link ExecutionHandler}
	 *
	 * @return the {@link ExecutionHandler}
	 */
	public ExecutionHandler getExecutionHandler() {
		return getComponent(ExecutionHandler.class);
	}

	/**
	 * Returns true if this execution policy was stopped
	 *
	 * @return true if this execution policy was stopped
	 */
	public boolean isStopped() {
		return this.stopped;
	}

	/**
	 * Returns the TX which is defined on this class, not the one defined on {@link StrolchPolicy}
	 *
	 * <p><b>Note:</b> This is used as execution policies can have a longer lifecycle than its transaction.</p>
	 *
	 * @return the TX which is defined on this class, not the one defined on {@link StrolchPolicy}
	 */
	@Override
	protected StrolchTransaction tx() {
		if (!this.tx.isOpen() && !this.tx.isCommitting())
			throw new IllegalStateException(
					"The TX is in state " + this.tx.getState() + " and can not be used anymore!");
		return this.tx;
	}

	/**
	 * Returns true if the current TX is still open
	 *
	 * @return true if the current TX is still open
	 */
	protected boolean isTxOpen() {
		return this.tx.isOpen() && !tx.isClosing();
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
	 * @param action the {@link Action} to check if it can be executed
	 *
	 * @return true if the action can be executed, false if not, i.e. the current state disallows the action to be
	 * executed
	 */
	public boolean isExecutable(Action action) {
		State state = action.getState();
		return state.isPlanned() || state.isStopped();
	}

	/**
	 * Performs any initialization of this {@link ExecutionPolicy} for the given action, this method stores the
	 * {@link Locator} of the action and its type
	 *
	 * @param action the action for which to initialize
	 */
	public void initialize(Action action) {
		this.actionType = action.getType();
		this.actionLoc = action.getLocator();
		this.resourceLoc = action.getResourceLocator();
	}

	/**
	 * Starts the execution of the given {@link Action}, i.e. sets the state to {@link State#EXECUTION}
	 *
	 * @param action the action to start execution for
	 */
	public abstract void toExecution(Action action);

	/**
	 * Completes execution of the given {@link Action}, i.e. sets the state to {@link State#EXECUTED}
	 *
	 * @param action the action to set to executed
	 */
	public abstract void toExecuted(Action action);

	/**
	 * Stops the execution of this {@link Action} without completing its execution, i.e. sets the state to
	 * {@link State#STOPPED}
	 *
	 * @param action the action to stop execution for
	 */
	public abstract void toStopped(Action action);

	/**
	 * Sets this {@link Action} which should be in execution to an error state, i.e. sets the state to
	 * {@link State#ERROR}
	 *
	 * @param action the action to set to error state
	 */
	public abstract void toError(Action action);

	/**
	 * Sets this {@link Action} which should be in execution to a warning state, i.e. sets the state to
	 * {@link State#WARNING}
	 *
	 * @param action the action to set to warning state
	 */
	public abstract void toWarning(Action action);

	/**
	 * Stops any active components in this {@link ExecutionPolicy}. This is used to notify the {@link ExecutionPolicy}
	 * that any scheduled actions should be stopped, that listening and observing registrations can be taken back, etc.
	 */
	public void stop() {
		this.stopped = true;
		try {
			this.futures.forEach(future -> future.cancel(false));
			handleStopped();
		} catch (Exception e) {
			logger.error("Stopping failed for " + this.actionLoc, e);
		}
	}

	/**
	 * Performs tasked required when this execution policy is stopped
	 */
	protected void handleStopped() {
		getDelayedExecutionTimer().cancel(this.actionLoc);
	}

	/**
	 * Updates the state of the given {@link Action} to the given {@link State} and updates the {@link Activity} for
	 * persisting on the TX
	 *
	 * @param action the action to change
	 * @param state  the new state to set
	 */
	protected void setActionState(Action action, State state) {
		if (action.getState().inClosedPhase())
			throw new IllegalStateException("Action " +
					action.getLocator() +
					" has state " +
					action.getState() +
					" and can not be changed to " +
					state);

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
	 * Finds the duration of the given {@link Action} by searching the activity hierarchy using
	 * {@link Action#findParameter(String, String, boolean)} using #BAG_OBJECTIVES and #PARAM_DURATION.
	 *
	 * @return the {@link DurationParameter}
	 */
	protected DurationParameter findActionDuration(Action action) {
		return action.findObjectivesParam(PARAM_DURATION, true);
	}

	/**
	 * Delays the given {@link Runnable} by the given {@link PeriodDuration}
	 *
	 * @param duration the duration to delay
	 * @param runnable the action to delay
	 */
	public void delay(PeriodDuration duration, Runnable runnable) {
		long delayMs = duration.toMillis();
		this.futures.add(getDelayedExecutionTimer().delay(delayMs, runnable));
	}

	/**
	 * Delays the given {@link Runnable} by the given {@link Duration}
	 *
	 * @param duration the duration to delay
	 * @param runnable the action to delay
	 */
	public void delay(Duration duration, Runnable runnable) {
		long delayMs = duration.toMillis();
		this.futures.add(getDelayedExecutionTimer().delay(delayMs, runnable));
	}

	/**
	 * Delays the given {@link Runnable} by the given duration, but randomly changing the duration in milliseconds by
	 * the given min and max factors
	 */
	protected void delayRandom(Duration duration, double minFactor, double maxFactor, Runnable runnable) {
		long durationMs = duration.toMillis();
		delayRandom(durationMs, minFactor, maxFactor, TimeUnit.MILLISECONDS, runnable);
	}

	/**
	 * Delays the given {@link Runnable} by the given duration, but randomly changing the duration in milliseconds by
	 * the given min and max factors
	 */
	protected void delayRandom(long duration, double minFactor, double maxFactor, TimeUnit delayUnit,
			Runnable runnable) {
		delayRandom((long) (duration * minFactor), (long) (duration * maxFactor), delayUnit, runnable);
	}

	/**
	 * Delays the given {@link Runnable} by randomly choosing a value by calling
	 * {@link ThreadLocalRandom#nextLong(long, long)} passing min and max as origin and bound respectively
	 */
	protected void delayRandom(long min, long max, TimeUnit delayUnit, Runnable runnable) {
		long delay = ThreadLocalRandom.current().nextLong(min, max + 1);
		delayRandom(delay, delayUnit, runnable);
	}

	/**
	 * Delays the given {@link Runnable} by the given delay value
	 *
	 * @param delay     the delay time
	 * @param delayUnit the UOM of the delay time
	 */
	protected void delayRandom(long delay, TimeUnit delayUnit, Runnable runnable) {
		long delayMs = delayUnit.toMillis(delay);
		if (delayMs < 20) {
			logger.warn("Delay time for " + this.actionLoc + " is less than 20ms, overriding!");
			delayMs = 20;
		}
		logger.info("Delaying runnable " + runnable + " by " + formatMillisecondsDuration(delayMs));
		this.futures.add(getDelayedExecutionTimer().delay(delayMs, runnable));
	}

	/**
	 * Async method to delay setting the given {@link Action} to executed by the duration defined by the
	 * {@link DurationParameter} found by calling {@link Action#findParameter(String, String, boolean)}
	 */
	protected void delayToExecuted(Action action) {
		delayToExecutedBy(findActionDuration(action));
	}

	/**
	 * Async method to delay setting the {@link Action} to executed by the duration defined by the given
	 * {@link DurationParameter}
	 */
	protected void delayToExecutedBy(DurationParameter durationP) {
		long duration = durationP.getValue().toMillis();
		delayToExecutedBy(duration, TimeUnit.MILLISECONDS);
	}

	/**
	 * Async method to delay setting the {@link Action} to executed by the given duration
	 */
	protected void delayToExecutedBy(Duration duration) {
		delayToExecutedBy(duration.toMillis(), TimeUnit.MILLISECONDS);
	}

	/**
	 * Async method to delay setting the {@link Action} to executed by the given duration, but randomly changing the
	 * duration in milliseconds by the given min and max factors
	 */
	protected void delayToExecutedByRandom(Duration duration, double minFactor, double maxFactor) {
		long durationMs = duration.toMillis();
		delayToExecutedByRandom(durationMs, minFactor, maxFactor, TimeUnit.MILLISECONDS);
	}

	/**
	 * Async method to delay setting the given {@link Action} to executed by the duration defined by the
	 * {@link DurationParameter} found by calling {@link Action#findParameter(String, String, boolean)}, but randomly
	 * changing the duration in milliseconds by the given min and max factors
	 */
	protected void delayToExecutedByRandom(Action action, double minFactor, double maxFactor) {
		delayToExecutedByRandom(findActionDuration(action), minFactor, maxFactor);
	}

	/**
	 * Async method to delay setting the {@link Action} to executed by the duration defined by the given
	 * {@link DurationParameter}, but randomly changing the duration in milliseconds by the given min and max factors
	 */
	protected void delayToExecutedByRandom(DurationParameter durationP, double minFactor, double maxFactor) {
		long duration = durationP.getValue().toMillis();
		delayToExecutedByRandom((long) (duration * minFactor), (long) (duration * maxFactor), TimeUnit.MILLISECONDS);
	}

	/**
	 * Async method to delay setting the {@link Action} to executed by the given duration, but randomly changing the
	 * duration in milliseconds by the given min and max factors
	 */
	protected void delayToExecutedByRandom(long duration, double minFactor, double maxFactor, TimeUnit delayUnit) {
		delayToExecutedByRandom((long) (duration * minFactor), (long) (duration * maxFactor), delayUnit);
	}

	/**
	 * Async method to delay setting the {@link Action} to executed by randomly choosing a value by calling
	 * {@link ThreadLocalRandom#nextLong(long, long)} passing min and max as origin and bound respectively
	 */
	protected void delayToExecutedByRandom(long min, long max, TimeUnit delayUnit) {
		long delay = ThreadLocalRandom.current().nextLong(min, max + 1);
		delayToExecutedBy(delay, delayUnit);
	}

	/**
	 * Async method to delay setting the {@link Action} to executed by the given delay value
	 *
	 * @param delay     the delay time
	 * @param delayUnit the UOM of the delay time
	 */
	protected void delayToExecutedBy(long delay, TimeUnit delayUnit) {
		long delayMs = delayUnit.toMillis(delay);
		logger.info("Delaying toExecuted of " + this.actionLoc + " by " + formatMillisecondsDuration(delayMs));
		getDelayedExecutionTimer().execute(this.realm, getContainer(), this.actionLoc, delayMs);
	}

	/**
	 * @return the {@link DelayedExecutionTimer} to simplify the delayed execution of an {@link Action}, e.g. for
	 * simulated execution or simple wait tasks
	 */
	protected DelayedExecutionTimer getDelayedExecutionTimer() {
		return getComponent(ExecutionHandler.class).getDelayedExecutionTimer();
	}

	/**
	 * Opens a {@link StrolchTransaction} where the realm retrieved using
	 * {@link ComponentContainer#getRealm(Certificate)}. This transaction should be used in a try-with-resource clause
	 * so it is properly closed.
	 *
	 * @param ctx      the privilege context
	 * @param readOnly true if this is a read-only TX
	 *
	 * @return the open {@link StrolchTransaction}
	 *
	 * @throws StrolchException if the {@link StrolchRealm} does not exist with the given name
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
	 * @param runnable the runnable to perform
	 *
	 * @throws PrivilegeException if the agent is missing the privilege
	 * @throws Exception          if anything else goes wrong during execution
	 */
	protected void runAsAgent(PrivilegedRunnable runnable) throws PrivilegeException, Exception {
		getContainer().getPrivilegeHandler().runAs(SYSTEM_USER_AGENT, runnable);
	}

	/**
	 * Performs the given {@link PrivilegedRunnableWithResult} as the system user
	 * {@link StrolchConstants#SYSTEM_USER_AGENT}
	 *
	 * @param runnable the runnable to perform
	 *
	 * @return the result of the operation
	 *
	 * @throws PrivilegeException if the agent is missing the privilege
	 * @throws Exception          if anything else goes wrong during execution
	 */
	protected <T> T runAsAgentWithResult(PrivilegedRunnableWithResult<T> runnable)
			throws PrivilegeException, Exception {
		return getContainer().getPrivilegeHandler().runWithResult(SYSTEM_USER_AGENT, runnable);
	}
}
