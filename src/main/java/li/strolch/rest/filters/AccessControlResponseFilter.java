package li.strolch.rest.filters;

import java.io.IOException;

import javax.annotation.Priority;
import javax.ws.rs.Priorities;
import javax.ws.rs.container.ContainerRequestContext;
import javax.ws.rs.container.ContainerResponseContext;
import javax.ws.rs.container.ContainerResponseFilter;
import javax.ws.rs.core.MultivaluedMap;
import javax.ws.rs.ext.Provider;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@Provider
@Priority(Priorities.HEADER_DECORATOR)
public class AccessControlResponseFilter implements ContainerResponseFilter {

	private static final Logger logger = LoggerFactory.getLogger(AccessControlResponseFilter.class);

	private static boolean corsEnabled;
	private static String origin;
	private static boolean logged;

	/**
	 * @param corsEnabled
	 *            the corsEnabled to set
	 */
	public static void setCorsEnabled(boolean corsEnabled) {
		AccessControlResponseFilter.corsEnabled = corsEnabled;
	}

	/**
	 * @param origin
	 *            the origin to set
	 */
	public static void setOrigin(String origin) {
		AccessControlResponseFilter.origin = origin;
	}

	@Override
	public void filter(ContainerRequestContext requestContext, ContainerResponseContext responseContext)
			throws IOException {

		if (!corsEnabled)
			return;

		if (!logged) {
			logged = true;
			logger.info("Enabling CORS for origin: " + origin);
		}

		MultivaluedMap<String, Object> headers = responseContext.getHeaders();

		headers.add("Access-Control-Allow-Origin", origin);
		headers.add("Access-Control-Allow-Headers", "Authorization, Origin, X-Requested-With, Content-Type");
		headers.add("Access-Control-Expose-Headers", "Location, Content-Disposition");
		headers.add("Access-Control-Allow-Methods", "POST, PUT, GET, DELETE, HEAD, OPTIONS");
	}
}