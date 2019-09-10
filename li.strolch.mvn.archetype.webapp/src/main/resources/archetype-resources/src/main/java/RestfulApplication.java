package ${package};

import javax.ws.rs.ApplicationPath;
import java.util.logging.Level;

import li.strolch.rest.RestfulStrolchComponent;
import li.strolch.rest.StrolchRestfulExceptionMapper;
import li.strolch.rest.endpoint.*;
import li.strolch.rest.filters.*;
import org.glassfish.jersey.logging.LoggingFeature;
import org.glassfish.jersey.server.ResourceConfig;
import org.glassfish.jersey.server.ServerProperties;

@ApplicationPath("rest")
public class RestfulApplication extends ResourceConfig {

	public RestfulApplication() {

		// ${appName} services
		//packages(GatewayResource.class.getPackage().getName());

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
		register(AuthenticationRequestFilter.class);
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
					Level.SEVERE, LoggingFeature.Verbosity.PAYLOAD_ANY, null));

			property(ServerProperties.TRACING, "ALL");
			property(ServerProperties.TRACING_THRESHOLD, "TRACE");
		}
	}
}
