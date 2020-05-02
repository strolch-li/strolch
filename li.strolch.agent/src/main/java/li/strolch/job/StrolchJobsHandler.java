package li.strolch.job;

import static java.lang.String.join;
import static li.strolch.runtime.StrolchConstants.*;

import java.lang.reflect.Constructor;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.StrolchAgent;
import li.strolch.agent.api.StrolchComponent;
import li.strolch.model.parameter.DateParameter;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Certificate;

/**
 * The {@link StrolchJobsHandler} registers all {@link StrolchJob StrolchJobs}
 */
public class StrolchJobsHandler extends StrolchComponent {

	protected Map<String, StrolchJob> jobs;

	/**
	 * Constructor which takes a reference to the container and the component's name under which it can be retrieved at
	 * runtime (although one mostly retrieves the component by interface class for automatic casting)
	 *
	 * @param container
	 * 		the container
	 * @param componentName
	 * 		the name of the component
	 */
	public StrolchJobsHandler(ComponentContainer container, String componentName) {
		super(container, componentName);
	}

	@Override
	public void start() throws Exception {
		reloadJobs();
		super.start();
	}

	public void reloadJobs() throws Exception {

		List<StrolchJob> jobs = new ArrayList<>();

		// copy any already existing programmatically added jobs
		if (this.jobs != null) {
			this.jobs.values().forEach(value -> {
				if (value.getConfigureMethod() == ConfigureMethod.Programmatic)
					jobs.add(value);
			});
		}

		String[] realmNames = getConfiguration().getStringArray("realms", join(",", getContainer().getRealmNames()));
		runAsAgent(ctx -> {
			for (String realmName : realmNames) {
				try (StrolchTransaction tx = openTx(ctx.getCertificate(), realmName, true)) {
					tx.streamResources(TYPE_STROLCH_JOB).forEach(jobRes -> {

						String className = jobRes.getParameter(PARAM_CLASS_NAME, true).getValue();
						String cron = jobRes.getParameter(PARAM_CRON, true).getValue();
						DateParameter startDateP = jobRes.getParameter(PARAM_START_DATE, true);
						JobMode mode = JobMode.valueOf(jobRes.getParameter(PARAM_MODE, true).getValue());

						StrolchJob job = instantiateJob(className, jobRes.getName(), mode);
						job.setConfigureMethod(ConfigureMethod.Model).setMode(mode)
								.setCronExpression(cron, startDateP.toZonedDateTime());

						jobs.add(job);
						logger.info("Added job " + job.getName() + ": " + job.getCron() + " from model.");
					});
				}
			}
		});

		StrolchAgent agent = getContainer().getAgent();
		ReloadJobsJob reloadJobsJob = new ReloadJobsJob(agent, ReloadJobsJob.class.getSimpleName(), JobMode.Manual);
		reloadJobsJob.setConfigureMethod(ConfigureMethod.Programmatic);
		jobs.add(reloadJobsJob);

		this.jobs = new HashMap<>();
		jobs.forEach(job -> internalRegister(job).schedule());
	}

	@SuppressWarnings("unchecked")
	private StrolchJob instantiateJob(String className, String name, JobMode mode) {
		Class<StrolchJob> clazz;
		try {
			clazz = (Class<StrolchJob>) Class.forName(className);
		} catch (ClassNotFoundException e) {
			throw new IllegalArgumentException("StrolchJob class " + className + " does not exist!");
		}

		return instantiateJob(clazz, name, mode);
	}

	private StrolchJob instantiateJob(Class<? extends StrolchJob> clazz, String name, JobMode mode) {
		StrolchJob strolchJob;
		try {
			Constructor<? extends StrolchJob> constructor = clazz
					.getConstructor(StrolchAgent.class, String.class, JobMode.class);
			strolchJob = constructor.newInstance(getContainer().getAgent(), name, mode);
		} catch (Exception e) {
			throw new IllegalArgumentException("Failed to instantiate job " + clazz.getName(), e);
		}
		return strolchJob;
	}

	public StrolchJob registerAndScheduleJob(Class<? extends StrolchJob> strolchJobClass) {
		StrolchJob job = instantiateJob(strolchJobClass, strolchJobClass.getSimpleName(), JobMode.Manual);
		job.setConfigureMethod(ConfigureMethod.Programmatic);
		return register(job).schedule();
	}

	public StrolchJob register(Class<? extends StrolchJob> strolchJobClass) {
		StrolchJob job = instantiateJob(strolchJobClass, strolchJobClass.getSimpleName(), JobMode.Manual);
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
