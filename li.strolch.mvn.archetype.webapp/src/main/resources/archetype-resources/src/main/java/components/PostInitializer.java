package ${package}.components;

import java.util.concurrent.TimeUnit;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.StrolchAgent;
import li.strolch.agent.impl.SimplePostInitializer;
import li.strolch.execution.ArchiveExecutedActivitiesJob;
import li.strolch.execution.ExecutionHandler;
import li.strolch.handler.mail.MailHandler;
import li.strolch.job.JobMode;
import li.strolch.job.StrolchJobsHandler;
import li.strolch.policy.ReloadPoliciesJob;
import li.strolch.policy.ReloadPrivilegeHandlerJob;
import li.strolch.runtime.configuration.RuntimeConfiguration;
import li.strolch.utils.helper.ExceptionHelper;

public class PostInitializer extends SimplePostInitializer {

	public PostInitializer(ComponentContainer container, String componentName) {
		super(container, componentName);
	}

	@Override
	public void start() throws Exception {

		registerJobs();
		notifyStart();

		super.start();
	}

	private void registerJobs() throws Exception {
		if (!getContainer().hasComponent(StrolchJobsHandler.class))
			return;

		StrolchJobsHandler jobsHandler = getComponent(StrolchJobsHandler.class);

		// Manually triggered jobs to run once on startup
		// jobsHandler.register(XXX.class).runNow();

		// special jobs which are triggered by an admin, and not run at startup
		jobsHandler.register(ReloadPoliciesJob.class);
		jobsHandler.register(ReloadPrivilegeHandlerJob.class);

		// recurring jobs
		// jobsHandler.registerAndScheduleJob(XXX.class);

		if (getContainer().hasComponent(ExecutionHandler.class)) {
			StrolchAgent agent = getContainer().getAgent();
			ArchiveExecutedActivitiesJob archiveExecutedActivitiesJob = new ArchiveExecutedActivitiesJob(agent,
					JobMode.Recurring, 5, TimeUnit.MINUTES, 6, TimeUnit.HOURS);
			jobsHandler.register(archiveExecutedActivitiesJob).runNow();
		}
	}

	private void notifyStart() {

		if (!(getConfiguration().getBoolean("notifyStart", Boolean.FALSE) && getContainer()
				.hasComponent(MailHandler.class)))
			return;

		String recipients = getConfiguration().getString("notifyStartRecipients", "");
		if (recipients.isEmpty()) {
			logger.error("config param notifyStartRecipients is empty, can not notify of boot!");
			return;
		}

		StrolchAgent agent = getContainer().getAgent();
		RuntimeConfiguration runtimeConfiguration = agent.getStrolchConfiguration().getRuntimeConfiguration();
		String subject = runtimeConfiguration.getApplicationName() + ":" + runtimeConfiguration.getEnvironment()
				+ " Startup Complete!";

		String body = "Dear User\n\n" //
				+ "The " + getConfiguration().getRuntimeConfiguration().getApplicationName()
				+ " Server has just completed startup with version " //
				+ agent.getVersion().getAppVersion().getArtifactVersion() //
				+ "\n\n" //
				+ "\tYour Server.";

		try {
			getContainer().getComponent(MailHandler.class).sendMail(subject, body, recipients);
		} catch (Exception e) {
			logger.error("Notifying of server startup failed: " + ExceptionHelper.getRootCause(e), e);
		}
	}
}
