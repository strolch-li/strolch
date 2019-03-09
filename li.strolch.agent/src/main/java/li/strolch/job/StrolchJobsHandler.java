package li.strolch.job;

import java.lang.reflect.Constructor;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.StrolchAgent;
import li.strolch.agent.api.StrolchComponent;
import li.strolch.privilege.model.Certificate;
import li.strolch.runtime.configuration.ComponentConfiguration;

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
	public void initialize(ComponentConfiguration configuration) throws Exception {
		this.jobs = new HashMap<>();
		super.initialize(configuration);
	}

	private StrolchJob instantiateStrolchJob(Class<? extends StrolchJob> strolchJobClass) {
		StrolchJob strolchJob;
		try {
			Constructor<? extends StrolchJob> constructor = strolchJobClass.getConstructor(StrolchAgent.class);
			strolchJob = constructor.newInstance(getContainer().getAgent());
		} catch (Exception e) {
			throw new IllegalArgumentException("Failed to instantiate class " + strolchJobClass.getName(), e);
		}
		return strolchJob;
	}

	public StrolchJob registerAndScheduleJob(Class<? extends StrolchJob> strolchJobClass) {
		return register(instantiateStrolchJob(strolchJobClass)).schedule();
	}

	public StrolchJob register(Class<? extends StrolchJob> strolchJobClass) {
		return register(instantiateStrolchJob(strolchJobClass));
	}

	public StrolchJob register(StrolchJob strolchJob) {
		if (this.jobs.containsKey(strolchJob.getName()))
			throw new IllegalArgumentException("Job " + strolchJob.getName() + " is already registered!");
		this.jobs.put(strolchJob.getName(), strolchJob);

		return strolchJob;
	}

	public List<StrolchJob> getJobs(Certificate cert, String source) {
		getContainer().getPrivilegeHandler().validate(cert, source).assertHasPrivilege(StrolchJobsHandler.class.getName());
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
