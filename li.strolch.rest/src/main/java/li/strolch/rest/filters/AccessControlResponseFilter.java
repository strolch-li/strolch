package li.strolch.rest.filters;

import java.io.IOException;
import java.text.MessageFormat;

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

	private static final String ACCESS_CONTROL_ALLOW_METHODS = "Access-Control-Allow-Methods"; //$NON-NLS-1$
	private static final String ACCESS_CONTROL_EXPOSE_HEADERS = "Access-Control-Expose-Headers"; //$NON-NLS-1$
	private static final String ACCESS_CONTROL_ALLOW_HEADERS = "Access-Control-Allow-Headers"; //$NON-NLS-1$
	private static final String ACCESS_CONTROL_ALLOW_ORIGIN = "Access-Control-Allow-Origin"; //$NON-NLS-1$

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
			logger.info(MessageFormat.format("Enabling CORS for origin: {0}", origin)); //$NON-NLS-1$
		}

		MultivaluedMap<String, Object> headers = responseContext.getHeaders();

		// allow for the configured origin
		headers.add(ACCESS_CONTROL_ALLOW_ORIGIN, origin);

		// and set the allowed HTTP headers and methods
		headers.add(ACCESS_CONTROL_ALLOW_HEADERS, "Authorization, Origin, X-Requested-With, Content-Type"); //$NON-NLS-1$
		headers.add(ACCESS_CONTROL_EXPOSE_HEADERS, "Location, Content-Disposition"); //$NON-NLS-1$
		headers.add(ACCESS_CONTROL_ALLOW_METHODS, "POST, PUT, GET, DELETE, HEAD, OPTIONS"); //$NON-NLS-1$
	}
}