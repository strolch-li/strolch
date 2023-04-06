package li.strolch.job;

import static java.time.ZoneId.systemDefault;
import static java.time.format.DateTimeFormatter.ISO_OFFSET_DATE_TIME;
import static li.strolch.model.Tags.AGENT;
import static li.strolch.runtime.StrolchConstants.DEFAULT_REALM;
import static li.strolch.runtime.StrolchConstants.SYSTEM_USER_AGENT;
import static li.strolch.utils.helper.StringHelper.formatMillisecondsDuration;
import static li.strolch.utils.helper.StringHelper.isEmpty;

import java.time.ZonedDateTime;
import java.time.temporal.ChronoUnit;
import java.util.ResourceBundle;
import java.util.concurrent.Future;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;

import com.google.gson.JsonObject;
import fc.cron.CronExpression;
import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.StrolchAgent;
import li.strolch.agent.api.StrolchComponent;
import li.strolch.agent.api.StrolchRealm;
import li.strolch.handler.operationslog.OperationsLog;
import li.strolch.model.Locator;
import li.strolch.model.Tags;
import li.strolch.model.log.LogMessage;
import li.strolch.model.log.LogMessageState;
import li.strolch.model.log.LogSeverity;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.base.PrivilegeException;
import li.strolch.privilege.model.Certificate;
import li.strolch.privilege.model.PrivilegeContext;
import li.strolch.privilege.model.Restrictable;
import li.strolch.runtime.StrolchConstants;
import li.strolch.runtime.privilege.PrivilegedRunnable;
import li.strolch.utils.helper.ExceptionHelper;
import li.strolch.utils.time.PeriodDuration;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * A StrolchJob is a simple job which performs an action. A StrolchJob can be scheduled so that it executes
 * periodically, or trigger externally e.g. from a UI. Subclasses must implement the
 */
public abstract class StrolchJob implements Runnable, Restrictable {

	protected static final Logger logger = LoggerFactory.getLogger(StrolchJob.class);

	private final StrolchAgent agent;
	private final String id;
	private final String name;
	private JobMode mode;

	private String cron;
	private CronExpression cronExpression;

	private String realmName;

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

	private ConfigureMethod configureMethod;
	private ZonedDateTime cronStartDate;

	public StrolchJob(StrolchAgent agent, String id, String name, JobMode jobMode) {
		this.agent = agent;
		this.id = id;
		this.name = name;
		this.mode = jobMode;
		this.first = true;
		this.configureMethod = ConfigureMethod.Programmatic;
	}

	public StrolchJob setConfigureMethod(ConfigureMethod configureMethod) {
		this.configureMethod = configureMethod;
		return this;
	}

	public ConfigureMethod getConfigureMethod() {
		return this.configureMethod;
	}

	public String getCron() {
		return this.cron;
	}

	public void setCronExpression(String cron, ZonedDateTime startDate) {
		this.cronExpression = CronExpression.createWithoutSeconds(cron);
		this.cron = cron;
		this.cronStartDate = startDate.isBefore(ZonedDateTime.now()) ? ZonedDateTime.now() : startDate;

		this.initialDelay = 0;
		this.initialDelayTimeUnit = null;
		this.delay = 0;
		this.delayTimeUnit = null;
	}

	public StrolchJob setDelay(long initialDelay, TimeUnit initialDelayTimeUnit, long delay, TimeUnit delayTimeUnit) {
		this.initialDelay = initialDelay;
		this.initialDelayTimeUnit = initialDelayTimeUnit;
		this.delay = delay;
		this.delayTimeUnit = delayTimeUnit;

		this.cronExpression = null;
		this.cron = null;
		this.cronStartDate = null;

		return this;
	}

	public JobMode getMode() {
		return this.mode;
	}

	public StrolchJob setMode(JobMode mode) {
		this.mode = mode;
		return this;
	}

	public String getId() {
		return this.id;
	}

	public String getName() {
		return this.name;
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
	 * Performs the given {@link PrivilegedRunnable} as the privileged system user
	 * {@link StrolchConstants#SYSTEM_USER_AGENT}
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
	 * Returns true if the given component is registered on this container
	 *
	 * @param clazz
	 * 		the type of component to check for
	 *
	 * @return true if the component is available
	 */
	public boolean hasComponent(Class<?> clazz) {
		return this.getContainer().hasComponent(clazz);
	}

	/**
	 * Opens a {@link StrolchTransaction} for the default realm and certificate
	 *
	 * @param cert
	 * 		the certificate authorizing the transaction
	 *
	 * @return the newly created transaction
	 */
	protected synchronized StrolchTransaction openTx(Certificate cert) {
		StrolchRealm realm = getContainer().getRealm(cert);
		this.realmName = realm.getRealm();
		return realm.openTx(cert, this.getClass(), false);
	}

	/**
	 * Opens a {@link StrolchTransaction} for the default realm and certificate
	 *
	 * @param cert
	 * 		the certificate authorizing the transaction
	 *
	 * @return the newly created transaction
	 */
	protected synchronized StrolchTransaction openTx(Certificate cert, boolean readOnly) {
		StrolchRealm realm = getContainer().getRealm(cert);
		this.realmName = realm.getRealm();
		return realm.openTx(cert, this.getClass(), readOnly);
	}

	/**
	 * Executes this job now, but if the job is currently running, then it is blocked till the job is complete
	 */
	public synchronized void runNow() throws Exception {
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
			logger.error("Execution of Job " + getName() + " failed.", e);

			if (getContainer().hasComponent(OperationsLog.class)) {
				OperationsLog operationsLog = getContainer().getComponent(OperationsLog.class);
				String realmName = this.realmName == null ? DEFAULT_REALM : this.realmName;
				operationsLog.addMessage(new LogMessage(realmName, SYSTEM_USER_AGENT,
						Locator.valueOf(AGENT, "strolch-agent", StrolchAgent.getUniqueId()), LogSeverity.Exception,
						LogMessageState.Information, ResourceBundle.getBundle("strolch-agent"),
						"job.failed").withException(e).value("jobName", getClass().getName()).value("reason", e));
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
	public synchronized StrolchJob schedule() {
		if (this.mode == JobMode.Manual) {
			logger.info("Not scheduling " + getName() + " as mode is " + this.mode);
			return this;
		}

		// first cancel a possibly already scheduled task
		cancel(false);

		if (this.first) {

			if (this.cronExpression != null) {
				ZonedDateTime executionTime;
				try {
					executionTime = this.cronExpression.nextTimeAfter(this.cronStartDate);
				} catch (IllegalArgumentException e) {
					logger.error("Can not schedule " + getName() + " after start date " + this.cronStartDate
							+ " as no next time exists for cron expression " + this.cron);
					return this;
				}

				logger.info("First execution of " + getName() + " will be at " + executionTime.format(
						ISO_OFFSET_DATE_TIME));

				long delay = PeriodDuration.between(ZonedDateTime.now(), executionTime).toMillis();
				this.future = getScheduledExecutor().schedule(this, delay, TimeUnit.MILLISECONDS);

			} else {

				long millis = this.initialDelayTimeUnit.toMillis(this.initialDelay);
				logger.info("First execution of " + getName() + " will be at " + ZonedDateTime.now()
						.plus(millis, ChronoUnit.MILLIS)
						.format(ISO_OFFSET_DATE_TIME));

				this.future = getScheduledExecutor().schedule(this, this.initialDelay, this.initialDelayTimeUnit);
			}

		} else {

			if (this.cronExpression != null) {
				ZonedDateTime executionTime;
				try {
					executionTime = this.cronExpression.nextTimeAfter(this.lastExecution);
				} catch (IllegalArgumentException e) {
					logger.error("Can not schedule " + getName() + " after start date " + this.lastExecution
							+ " as no next time exists for cron expression " + this.cron);
					return this;
				}

				logger.info(
						"Next execution of " + getName() + " will be at " + executionTime.format(ISO_OFFSET_DATE_TIME));

				long delay = PeriodDuration.between(ZonedDateTime.now(), executionTime).toMillis();
				this.future = getScheduledExecutor().schedule(this, delay, TimeUnit.MILLISECONDS);

			} else {

				long millis = this.delayTimeUnit.toMillis(this.delay);
				logger.info("Next execution of " + getName() + " will be at " + ZonedDateTime.now()
						.plus(millis, ChronoUnit.MILLIS)
						.format(ISO_OFFSET_DATE_TIME));

				this.future = getScheduledExecutor().schedule(this, this.delay, this.delayTimeUnit);
			}
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

	public synchronized JsonObject toJson() {
		JsonObject jsonObject = new JsonObject();

		jsonObject.addProperty(Tags.Json.ID, this.id);
		jsonObject.addProperty(Tags.Json.NAME, this.name);
		jsonObject.addProperty(Tags.Json.REALM, this.realmName);
		jsonObject.addProperty("mode", this.mode.name());
		jsonObject.addProperty("configureMethod", this.configureMethod == null ? "-" : this.configureMethod.name());
		jsonObject.addProperty("cron", this.cron == null ? "-" : this.cron);
		jsonObject.addProperty("initialDelay", this.initialDelay);
		jsonObject.addProperty("initialDelayTimeUnit",
				this.initialDelayTimeUnit == null ? "-" : this.initialDelayTimeUnit.name());
		jsonObject.addProperty("delay", this.delay);
		jsonObject.addProperty("delayTimeUnit", this.delayTimeUnit == null ? "-" : this.delayTimeUnit.name());

		jsonObject.addProperty("running", this.running);
		jsonObject.addProperty("totalDuration", formatMillisecondsDuration(this.totalDuration));
		jsonObject.addProperty("lastDuration", formatMillisecondsDuration(this.lastDuration));

		if (this.lastExecution == null) {
			jsonObject.addProperty("lastExecution", "-");
		} else {
			jsonObject.addProperty("lastExecution",
					this.lastExecution.format(ISO_OFFSET_DATE_TIME.withZone(systemDefault())));
		}

		if (this.future == null) {
			jsonObject.addProperty("nextExecution", "-");
		} else {

			long delay = this.future.getDelay(TimeUnit.MILLISECONDS);
			String nextExecution = ZonedDateTime.now()
					.plus(delay, ChronoUnit.MILLIS)
					.format(ISO_OFFSET_DATE_TIME.withZone(systemDefault()));
			jsonObject.addProperty("nextExecution", nextExecution);
		}

		jsonObject.addProperty("nrOfExecutions", this.nrOfExecutions);

		Exception lastException = this.lastException;
		if (lastException != null)
			jsonObject.addProperty("lastException", ExceptionHelper.formatExceptionMessage(lastException));

		return jsonObject;
	}

	@Override
	public String toString() {
		String schedule;
		if (this.mode == JobMode.Manual)
			schedule = this.mode.name();
		else if (isEmpty(this.cron))
			schedule = this.mode.name() + " Delay: " + this.initialDelay + " " + this.initialDelayTimeUnit + ", "
					+ this.delay + " " + this.delayTimeUnit;
		else
			schedule = this.mode.name() + " " + this.cron;

		return "Job " + this.id + " / " + this.name + " @ " + schedule;
	}
}
