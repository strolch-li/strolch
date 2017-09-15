package li.strolch.job;

import java.util.concurrent.Future;
import java.util.concurrent.ScheduledExecutorService;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.StrolchAgent;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.base.PrivilegeException;
import li.strolch.privilege.model.Certificate;
import li.strolch.runtime.StrolchConstants;
import li.strolch.runtime.privilege.PrivilegedRunnable;

public abstract class StrolchJob implements Runnable {

	protected static final Logger logger = LoggerFactory.getLogger(StrolchJob.class);

	private StrolchAgent agent;

	public StrolchJob(StrolchAgent agent) {
		this.agent = agent;
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
		return getAgent().getScheduledExecutor();
	}

	/**
	 * Performs the given {@link PrivilegedRunnable} as the privileged system user
	 * {@link StrolchConstants#SYSTEM_USER_AGENT}
	 * 
	 * @param runnable
	 *            the runnable to perform
	 * 
	 * @throws PrivilegeException
	 */
	protected void runAsAgent(PrivilegedRunnable runnable) throws PrivilegeException {
		getContainer().getPrivilegeHandler().runAsAgent(runnable);
	}

	/**
	 * Opens a {@link StrolchTransaction} for the default realm and certificate
	 * 
	 * @param cert
	 *            the certificate authorizing the transaction
	 * 
	 * @return the newly created transaction
	 */
	protected StrolchTransaction openTx(Certificate cert) {
		return getContainer().getRealm(cert).openTx(cert, this.getClass());
	}

	public abstract Future<?> schedule();
}
