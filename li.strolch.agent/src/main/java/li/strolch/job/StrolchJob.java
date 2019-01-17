package li.strolch.job;

import static li.strolch.model.Tags.AGENT;
import static li.strolch.runtime.StrolchConstants.SYSTEM_USER_AGENT;
import static li.strolch.utils.helper.StringHelper.formatMillisecondsDuration;

import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;
import java.util.ResourceBundle;
import java.util.concurrent.Future;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;

import com.google.gson.JsonObject;
import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.StrolchAgent;
import li.strolch.agent.api.StrolchComponent;
import li.strolch.agent.api.StrolchRealm;
import li.strolch.handler.operationslog.LogMessage;
import li.strolch.handler.operationslog.LogSeverity;
import li.strolch.handler.operationslog.OperationsLog;
import li.strolch.model.Locator;
import li.strolch.model.Tags;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.base.PrivilegeException;
import li.strolch.privilege.model.Certificate;
import li.strolch.privilege.model.PrivilegeContext;
import li.strolch.privilege.model.Restrictable;
import li.strolch.runtime.StrolchConstants;
import li.strolch.runtime.privilege.PrivilegedRunnable;
import li.strolch.utils.helper.ExceptionHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * A StrolchJob is a simple job which performs an action. A StrolchJob can be scheduled so that it executes
 * periodically, or trigger externally e.g. from a UI. Sub classes must implement the
 */
public abstract class StrolchJob implements Runnable, Restrictable {

	protected static final Logger logger = LoggerFactory.getLogger(StrolchJob.class);

	private StrolchAgent agent;
	private String realmName;

	private final JobMode mode;
	private long initialDelay;
	private TimeUnit initialDelayTimeUnit;
	private long delay;
	private TimeUnit delayTimeUnit;

	private boolean running;
	private long nrOfExecutions;
	private long totalDuration;
	private long lastDuration;
	private boolean first;
	private ScheduledFuture<?> future;
	private ZonedDateTime lastExecution;
	private Exception lastException;

	public StrolchJob(StrolchAgent agent) {
		this.agent = agent;
		this.mode = JobMode.Manual;

		this.initialDelay = 0;
		this.initialDelayTimeUnit = TimeUnit.SECONDS;
		this.delay = 0;
		this.delayTimeUnit = TimeUnit.SECONDS;

		this.first = true;
	}

	public StrolchJob(StrolchAgent agent, JobMode jobMode, long initialDelay, TimeUnit initialDelayTimeUnit, long delay,
			TimeUnit delayTimeUnit) {
		this.agent = agent;
		this.mode = jobMode;

		this.initialDelay = initialDelay;
		this.initialDelayTimeUnit = initialDelayTimeUnit;
		this.delay = delay;
		this.delayTimeUnit = delayTimeUnit;

		this.first = true;
	}

	public JobMode getMode() {
		return this.mode;
	}

	public String getName() {
		return getClass().getSimpleName();
	}

	protected StrolchAgent getAgent() {
		return this.agent;
	}

	public long getInitialDelay() {
		return this.initialDelay;
	}

	public void setInitialDelay(long initialDelay) {
		this.initialDelay = initialDelay;
	}

	public TimeUnit getInitialDelayTimeUnit() {
		return this.initialDelayTimeUnit;
	}

	public void setInitialDelayTimeUnit(TimeUnit initialDelayTimeUnit) {
		this.initialDelayTimeUnit = initialDelayTimeUnit;
	}

	public long getDelay() {
		return this.delay;
	}

	public void setDelay(long delay) {
		this.delay = delay;
	}

	public TimeUnit getDelayTimeUnit() {
		return this.delayTimeUnit;
	}

	public void setDelayTimeUnit(TimeUnit delayTimeUnit) {
		this.delayTimeUnit = delayTimeUnit;
	}

	protected ComponentContainer getContainer() {
		return getAgent().getContainer();
	}

	protected ScheduledExecutorService getScheduledExecutor() {
		return getAgent().getScheduledExecutor("StrolchJob");
	}

	/**
	 * Performs the given {@link PrivilegedRunnable} as the privileged system user {@link
	 * StrolchConstants#SYSTEM_USER_AGENT}
	 *
	 * @param runnable
	 * 		the runnable to perform
	 *
	 * @throws PrivilegeException
	 * 		if the agent can not perform the action
	 * @throws Exception
	 * 		if anything else goes wrong during execution
	 */
	protected void runAsAgent(PrivilegedRunnable runnable) throws PrivilegeException, Exception {
		getContainer().getPrivilegeHandler().runAsAgent(runnable);
	}

	/**
	 * Returns the reference to the {@link StrolchComponent} with the given name, if it exists. If it does not exist, an
	 * {@link IllegalArgumentException} is thrown
	 *
	 * @param clazz
	 * 		the type of component to return
	 *
	 * @return the component with the given name
	 *
	 * @throws IllegalArgumentException
	 * 		if the component does not exist
	 */
	public <T> T getComponent(Class<T> clazz) throws IllegalArgumentException {
		return getContainer().getComponent(clazz);
	}

	/**
	 * Opens a {@link StrolchTransaction} for the default realm and certificate
	 *
	 * @param cert
	 * 		the certificate authorizing the transaction
	 *
	 * @return the newly created transaction
	 */
	protected StrolchTransaction openTx(Certificate cert) {
		StrolchRealm realm = getContainer().getRealm(cert);
		this.realmName = realm.getRealm();
		return realm.openTx(cert, this.getClass());
	}

	/**
	 * Executes this job now, but if the job is currently running, then it is blocked till the job is complete
	 */
	public void runNow() throws Exception {
		doWork();
		schedule();
		if (this.lastException != null)
			throw this.lastException;
	}

	private synchronized void doWork() {
		this.running = true;
		long start = System.currentTimeMillis();

		try {
			runAsAgent(this::execute);
			this.lastException = null;
		} catch (Exception e) {
			this.running = false;
			this.lastException = e;
			logger.error("Execution of Job " + this.getClass().getSimpleName() + " failed.", e);

			OperationsLog operationsLog = getContainer().getComponent(OperationsLog.class);
			if (operationsLog != null) {
				operationsLog.addMessage(
						new LogMessage(this.realmName == null ? StrolchConstants.DEFAULT_REALM : this.realmName,
								SYSTEM_USER_AGENT, Locator.valueOf(AGENT, "strolch-agent", StrolchAgent.getUniqueId()),
								LogSeverity.Exception, ResourceBundle.getBundle("strolch-agent"), "strolchjob.failed")
								.withException(e).value("jobName", getClass().getName()).value("reason", e));
			}
		}

		long took = System.currentTimeMillis() - start;
		this.totalDuration += took;
		this.lastDuration = took;
		this.running = false;
		this.lastExecution = ZonedDateTime.now();
		this.nrOfExecutions++;
	}

	@Override
	public final void run() {

		doWork();

		if (this.first) {
			this.first = false;

			if (this.mode == JobMode.Recurring) {
				schedule();
			} else {
				logger.info("Not scheduling " + getName() + " after first execution as mode is " + this.mode);
			}

		} else {
			schedule();
		}
	}

	/**
	 * <p>Cancels this job, if it is already scheduled</p>
	 *
	 * @return this instance for chaining
	 *
	 * @see Future#cancel(boolean)
	 */
	public StrolchJob cancel(boolean mayInterruptIfRunning) {
		if (this.future != null) {
			this.future.cancel(mayInterruptIfRunning);
			this.future = null;
		}

		return this;
	}

	/**
	 * <p>Schedules this job by using the currently set delay parameters. If the job hasn't been executed yet, then the
	 * initial delay parameters are used, otherwise the delay parameters are used.</p>
	 *
	 * <p>If a job is currently scheduled, then that schedule will be cancelled and a new schedule is defined</p>
	 *
	 * @return this instance for chaining
	 */
	public StrolchJob schedule() {
		if (this.mode == JobMode.Manual) {
			logger.info("Not scheduling " + getName() + " as mode is " + this.mode);
			return this;
		}

		// first cancel a possibly already scheduled task
		cancel(false);

		if (this.first) {
			long millis = this.initialDelayTimeUnit.toMillis(this.initialDelay);
			logger.info("First execution of " + getClass().getSimpleName() + " will be at " + ZonedDateTime.now()
					.plus(millis, ChronoUnit.MILLIS).format(DateTimeFormatter.ISO_OFFSET_DATE_TIME));

			this.future = getScheduledExecutor().schedule(this, this.initialDelay, this.initialDelayTimeUnit);

		} else {
			long millis = this.delayTimeUnit.toMillis(this.delay);
			logger.info("Next execution of " + getClass().getSimpleName() + " will be at " + ZonedDateTime.now()
					.plus(millis, ChronoUnit.MILLIS).format(DateTimeFormatter.ISO_OFFSET_DATE_TIME));

			this.future = getScheduledExecutor().schedule(this, this.delay, this.delayTimeUnit);
		}

		return this;
	}

	protected abstract void execute(PrivilegeContext ctx) throws Exception;

	@Override
	public String getPrivilegeName() {
		return StrolchJob.class.getName();
	}

	@Override
	public Object getPrivilegeValue() {
		return this.getClass().getName();
	}

	public JsonObject toJson() {
		JsonObject jsonObject = new JsonObject();

		jsonObject.addProperty(Tags.Json.NAME, getName());
		jsonObject.addProperty(Tags.Json.REALM, this.realmName);
		jsonObject.addProperty("mode", this.mode.name());
		jsonObject.addProperty("initialDelay", this.initialDelay);
		jsonObject.addProperty("initialDelayTimeUnit", this.initialDelayTimeUnit.name());
		jsonObject.addProperty("delay", this.delay);
		jsonObject.addProperty("delayTimeUnit", this.delayTimeUnit.name());

		jsonObject.addProperty("running", this.running);
		jsonObject.addProperty("totalDuration", formatMillisecondsDuration(totalDuration));
		jsonObject.addProperty("lastDuration", formatMillisecondsDuration(lastDuration));

		if (this.lastExecution == null) {
			jsonObject.addProperty("lastExecution", "-");
		} else {
			String lastExecution = this.lastExecution
					.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME.withZone(ZoneId.systemDefault()));
			jsonObject.addProperty("lastExecution", lastExecution);
		}

		if (this.future == null) {
			jsonObject.addProperty("nextExecution", "-");
		} else {

			long delay = this.future.getDelay(TimeUnit.MILLISECONDS);
			String nextExecution = ZonedDateTime.now().plus(delay, ChronoUnit.MILLIS)
					.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME.withZone(ZoneId.systemDefault()));
			jsonObject.addProperty("nextExecution", nextExecution);
		}

		jsonObject.addProperty("nrOfExecutions", this.nrOfExecutions);

		if (this.lastException != null)
			jsonObject.addProperty("lastException", ExceptionHelper.formatExceptionMessage(this.lastException));

		return jsonObject;
	}
}
