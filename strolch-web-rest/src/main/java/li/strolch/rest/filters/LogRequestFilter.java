package li.strolch.rest.filters;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.ws.rs.container.*;
import jakarta.ws.rs.core.Context;
import jakarta.ws.rs.core.Response;
import li.strolch.rest.RestfulStrolchComponent;
import li.strolch.rest.helper.RestfulHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;

import static li.strolch.rest.StrolchRestfulConstants.STROLCH_REMOTE_IP;
import static li.strolch.rest.StrolchRestfulConstants.STROLCH_REQUEST_URL;
import static li.strolch.rest.helper.ServletRequestHelper.logRequest;

@PreMatching
public class LogRequestFilter implements ContainerRequestFilter, ContainerResponseFilter {

	private static final Logger logger = LoggerFactory.getLogger(LogRequestFilter.class);

	@Context
	private HttpServletRequest request;

	@Override
	public void filter(ContainerRequestContext requestContext) throws IOException {
		String remoteIp = RestfulHelper.getRemoteIp(this.request);
		if (RestfulStrolchComponent.getInstance().isRestLogging())
			logger.info("Remote IP: {}: {} {}", remoteIp, requestContext.getMethod(),
					requestContext.getUriInfo().getRequestUri());

		this.request.setAttribute(STROLCH_REMOTE_IP, remoteIp);
		this.request.setAttribute(STROLCH_REQUEST_URL,
				requestContext.getMethod() + " " + requestContext.getUriInfo().getRequestUri());

		logRequest(this.request);
	}

	@Override
	public void filter(ContainerRequestContext requestContext, ContainerResponseContext responseContext)
			throws IOException {
		int status = responseContext.getStatus();
		if (status != Response.Status.OK.getStatusCode()) {
			String method = requestContext.getMethod();
			String uri = request.getRequestURI();

			logger.error("Request failed {}: {} {}", responseContext.getStatus(), method, uri);
		}
	}
}
