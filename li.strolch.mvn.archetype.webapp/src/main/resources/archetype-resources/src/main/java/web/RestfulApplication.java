package ${package}.web;

import static ${package}.web.StartupListener.APP_NAME;

import javax.ws.rs.ApplicationPath;
import javax.ws.rs.Priorities;
import java.util.logging.Level;

import ${package}.rest.BooksResource;
import li.strolch.rest.RestfulStrolchComponent;
import li.strolch.rest.StrolchRestfulExceptionMapper;
import li.strolch.rest.endpoint.*;
import li.strolch.rest.filters.*;
import org.glassfish.jersey.logging.LoggingFeature;
import org.glassfish.jersey.server.ResourceConfig;
import org.glassfish.jersey.server.ServerProperties;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@ApplicationPath("rest")
public class RestfulApplication extends ResourceConfig {

	private static final Logger logger = LoggerFactory.getLogger(RestfulApplication.class);

	public RestfulApplication() {
		setApplicationName(APP_NAME);

		// add project resources by package name
		packages(BooksResource.class.getPackage().getName());

		// strolch services
		register(AuthenticationService.class);
		register(StrolchJobsResource.class);
		register(ReportResource.class);
		register(ControlResource.class);
		register(EnumQuery.class);
		register(Inspector.class);
		register(UserSessionsService.class);
		register(PrivilegeUsersService.class);
		register(PrivilegeRolesService.class);
		register(PrivilegePoliciesService.class);
		register(OperationsLogResource.class);
		register(VersionQuery.class);

		// filters
		register(AuthenticationRequestFilter.class, Priorities.AUTHENTICATION);
		register(AccessControlResponseFilter.class);
		register(AuthenticationResponseFilter.class);
		register(HttpCacheResponseFilter.class);

		// log exceptions and return them as plain text to the caller
		register(StrolchRestfulExceptionMapper.class);

		// the JSON generated is in UTF-8
		register(CharsetResponseFilter.class);

		RestfulStrolchComponent restfulComponent = RestfulStrolchComponent.getInstance();
		if (restfulComponent.isRestLogging()) {
			register(new LoggingFeature(java.util.logging.Logger.getLogger(LoggingFeature.DEFAULT_LOGGER_NAME),
					Level.SEVERE, LoggingFeature.Verbosity.PAYLOAD_ANY, Integer.MAX_VALUE));

			property(ServerProperties.TRACING, "ALL");
			property(ServerProperties.TRACING_THRESHOLD, "TRACE");
		}

		logger.info(
				"Initialized REST application " + getApplicationName() + " with " + getClasses().size() + " classes, "
						+ getInstances().size() + " instances, " + getResources().size() + " resources and "
						+ getProperties().size() + " properties");
	}
}
