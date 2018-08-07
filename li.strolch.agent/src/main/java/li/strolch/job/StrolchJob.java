package li.strolch.job;

import static li.strolch.model.Tags.AGENT;

import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;
import java.util.ResourceBundle;
import java.util.concurrent.Future;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.StrolchAgent;
import li.strolch.agent.api.StrolchRealm;
import li.strolch.handler.operationslog.LogMessage;
import li.strolch.handler.operationslog.LogSeverity;
import li.strolch.handler.operationslog.OperationsLog;
import li.strolch.model.Locator;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.base.PrivilegeException;
import li.strolch.privilege.model.Certificate;
import li.strolch.privilege.model.PrivilegeContext;
import li.strolch.runtime.StrolchConstants;
import li.strolch.runtime.privilege.PrivilegedRunnable;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public abstract class StrolchJob implements Runnable {

	protected static final Logger logger = LoggerFactory.getLogger(StrolchJob.class);

	private StrolchAgent agent;
	private String realmName;
	private boolean first;
	private Future<?> future;

	public StrolchJob(StrolchAgent agent) {
		this.agent = agent;
		this.first = true;
	}

	/**
	 * @return the agent
	 */
	protected StrolchAgent getAgent() {
		return this.agent;
	}

	/**
	 * @return the {@link ComponentContainer}
	 */
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
	 */
	protected void runAsAgent(PrivilegedRunnable runnable) throws PrivilegeException {
		getContainer().getPrivilegeHandler().runAsAgent(runnable);
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

	@Override
	public final void run() {
		try {
			runAsAgent(this::execute);
		} catch (Throwable e) {
			logger.error("Execution of Job " + this.getClass().getSimpleName() + " failed.", e);

			OperationsLog operationsLog = getContainer().getComponent(OperationsLog.class);
			if (operationsLog != null) {
				operationsLog.addMessage(
						new LogMessage(this.realmName == null ? StrolchConstants.DEFAULT_REALM : this.realmName,
								Locator.valueOf(AGENT, "strolch-agent", StrolchAgent.getUniqueId()),
								LogSeverity.Exception, ResourceBundle.getBundle("strolch-agent"),
								"strolchjob.failed").withException(e).value("jobName", getClass().getName())
								.value("reason", e));
			}
		}

		this.first = false;
		schedule();
	}

	public void cancel(boolean mayInterruptIfRunning) {
		if (this.future != null)
			this.future.cancel(mayInterruptIfRunning);
	}

	protected abstract long getInitialDelay();

	protected abstract TimeUnit getInitialDelayTimeUnit();

	protected abstract long getDelay();

	protected abstract TimeUnit getDelayTimeUnit();

	public StrolchJob schedule() {

		if (this.first) {
			long millis = getInitialDelayTimeUnit().toMillis(getInitialDelay());
			logger.info("First execution of " + getClass().getSimpleName() + " will be at " + ZonedDateTime.now()
					.plus(millis, ChronoUnit.MILLIS).format(DateTimeFormatter.ISO_OFFSET_DATE_TIME));

			this.future = getScheduledExecutor().schedule(this, getInitialDelay(), getInitialDelayTimeUnit());

		} else {
			long millis = getDelayTimeUnit().toMillis(getDelay());
			logger.info("Next execution of " + getClass().getSimpleName() + " will be at " + ZonedDateTime.now()
					.plus(millis, ChronoUnit.MILLIS).format(DateTimeFormatter.ISO_OFFSET_DATE_TIME));

			this.future = getScheduledExecutor().schedule(this, getDelay(), getDelayTimeUnit());
		}

		return this;
	}

	protected abstract void execute(PrivilegeContext ctx);
}
