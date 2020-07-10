package li.strolch.job;

import static java.lang.String.join;
import static li.strolch.runtime.StrolchConstants.*;

import java.lang.reflect.Constructor;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.StrolchAgent;
import li.strolch.agent.api.StrolchComponent;
import li.strolch.model.Resource;
import li.strolch.model.StrolchModelConstants;
import li.strolch.model.parameter.DateParameter;
import li.strolch.model.parameter.IntegerParameter;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Certificate;

/**
 * The {@link StrolchJobsHandler} registers all {@link StrolchJob StrolchJobs}
 */
public class StrolchJobsHandler extends StrolchComponent {

	protected Map<String, StrolchJob> jobs;

	public StrolchJobsHandler(ComponentContainer container, String componentName) {
		super(container, componentName);
	}

	@Override
	public void start() throws Exception {
		reloadJobs(true);
		super.start();
	}

	public void reloadJobs() throws Exception {
		reloadJobs(false);
	}

	private void reloadJobs(boolean catchExceptions) throws Exception {

		List<StrolchJob> jobs = new ArrayList<>();

		if (this.jobs != null) {
			this.jobs.values().forEach(value -> {

				// cancel any schedule jobs
				value.cancel(false);

				// copy any already existing programmatically added jobs
				if (value.getConfigureMethod() == ConfigureMethod.Programmatic)
					jobs.add(value);
			});
		}

		String[] realmNames = getConfiguration().getStringArray("realms", join(",", getContainer().getRealmNames()));
		runAsAgent(ctx -> {
			for (String realmName : realmNames) {
				try (StrolchTransaction tx = openTx(ctx.getCertificate(), realmName, true)) {
					tx.streamResources(TYPE_STROLCH_JOB).forEach(jobRes -> {
						loadJob(jobs, jobRes, catchExceptions);
					});
				}
			}
		});

		StrolchAgent agent = getContainer().getAgent();
		ReloadJobsJob reloadJobsJob = new ReloadJobsJob(agent, ReloadJobsJob.class.getSimpleName(),
				ReloadJobsJob.class.getSimpleName(), JobMode.Manual);
		reloadJobsJob.setConfigureMethod(ConfigureMethod.Model);
		jobs.add(reloadJobsJob);

		this.jobs = new HashMap<>();
		jobs.forEach(job -> internalRegister(job).schedule());
	}

	private void loadJob(List<StrolchJob> jobs, Resource jobRes, boolean catchExceptions) {
		try {
			String className = jobRes.getParameter(StrolchModelConstants.PARAM_CLASS_NAME, true).getValue();
			JobMode mode = JobMode.valueOf(jobRes.getParameter(StrolchModelConstants.PARAM_MODE, true).getValue());

			StrolchJob job = instantiateJob(className, jobRes.getId(), jobRes.getName(), mode);
			job.setConfigureMethod(ConfigureMethod.Model).setMode(mode);

			if (mode == JobMode.Manual) {
				jobs.add(job);
				logger.info("Added job " + job + " from model.");
				return;
			}

			if (jobRes.hasParameter(StrolchModelConstants.PARAM_CRON)) {
				String cron = jobRes.getParameter(StrolchModelConstants.PARAM_CRON, true).getValue();
				DateParameter startDateP = jobRes.getParameter(StrolchModelConstants.PARAM_START_DATE, true);
				job.setCronExpression(cron, startDateP.getValueZdt());

				jobs.add(job);
				logger.info("Added job " + job + " from model.");

				return;
			}

			if (jobRes.hasParameter(StrolchModelConstants.PARAM_INITIAL_DELAY) && jobRes
					.hasParameter(StrolchModelConstants.PARAM_DELAY)) {
				IntegerParameter initialDelayP = jobRes.getParameter(StrolchModelConstants.PARAM_INITIAL_DELAY, true);
				IntegerParameter delayP = jobRes.getParameter(StrolchModelConstants.PARAM_DELAY, true);
				TimeUnit initialDelayUnit = TimeUnit.valueOf(initialDelayP.getUom());
				TimeUnit delayUnit = TimeUnit.valueOf(delayP.getUom());
				job.setDelay(initialDelayP.getValue(), initialDelayUnit, delayP.getValue(), delayUnit);

				jobs.add(job);
				logger.info("Added job " + job + " from model.");

				return;
			}

			logger.error(
					"Job " + jobRes.getId() + " is inconsistent, as either cron, or initialDelay/delay is missing!");
		} catch (RuntimeException e) {
			if (catchExceptions)
				logger.error("Failed to load StrolchJob " + jobRes.getId() + " from model", e);
			else
				throw new IllegalStateException("Failed to load StrolchJob " + jobRes.getId() + " from model", e);
		}
	}

	@SuppressWarnings("unchecked")
	private StrolchJob instantiateJob(String className, String id, String name, JobMode mode) {
		Class<StrolchJob> clazz;
		try {
			clazz = (Class<StrolchJob>) Class.forName(className);
		} catch (ClassNotFoundException e) {
			throw new IllegalArgumentException("StrolchJob class " + className + " does not exist!");
		}

		return instantiateJob(clazz, id, name, mode);
	}

	private StrolchJob instantiateJob(Class<? extends StrolchJob> clazz, String id, String name, JobMode mode) {
		StrolchJob strolchJob;
		try {
			Constructor<? extends StrolchJob> constructor = clazz
					.getConstructor(StrolchAgent.class, String.class, String.class, JobMode.class);
			strolchJob = constructor.newInstance(getContainer().getAgent(), id, name, mode);
		} catch (Exception e) {
			throw new IllegalArgumentException("Failed to instantiate job " + clazz.getName(), e);
		}
		return strolchJob;
	}

	public StrolchJob registerAndScheduleJob(Class<? extends StrolchJob> strolchJobClass) {
		StrolchJob job = instantiateJob(strolchJobClass, strolchJobClass.getSimpleName(),
				strolchJobClass.getSimpleName(), JobMode.Manual);
		job.setConfigureMethod(ConfigureMethod.Programmatic);
		return register(job).schedule();
	}

	public StrolchJob register(Class<? extends StrolchJob> strolchJobClass) {
		StrolchJob job = instantiateJob(strolchJobClass, strolchJobClass.getSimpleName(),
				strolchJobClass.getSimpleName(), JobMode.Manual);
		job.setConfigureMethod(ConfigureMethod.Programmatic);
		return register(job);
	}

	public StrolchJob register(StrolchJob job) {
		return internalRegister(job.setConfigureMethod(ConfigureMethod.Programmatic));
	}

	private StrolchJob internalRegister(StrolchJob job) {
		if (this.jobs.containsKey(job.getName()))
			throw new IllegalArgumentException("Job " + job.getName() + " is already registered!");
		this.jobs.put(job.getName(), job);

		return job;
	}

	public List<StrolchJob> getJobs(Certificate cert, String source) {
		getContainer().getPrivilegeHandler().validate(cert, source)
				.assertHasPrivilege(StrolchJobsHandler.class.getName());
		return new ArrayList<>(this.jobs.values());
	}

	public StrolchJob getJob(Certificate cert, String source, String jobName) {
		getContainer().getPrivilegeHandler().validate(cert, source)
				.assertHasPrivilege(StrolchJobsHandler.class.getName());
		StrolchJob strolchJob = this.jobs.get(jobName);
		if (strolchJob == null)
			throw new IllegalArgumentException("Job " + jobName + " is not registered!");
		return strolchJob;
	}

	@Override
	public void stop() throws Exception {

		for (StrolchJob job : this.jobs.values()) {
			job.cancel(true);
		}

		super.stop();
	}

	@Override
	public void destroy() throws Exception {
		this.jobs.clear();
		super.destroy();
	}
}
