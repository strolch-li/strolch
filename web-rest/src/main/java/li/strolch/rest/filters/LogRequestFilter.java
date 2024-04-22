package li.strolch.rest.filters;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.ws.rs.container.ContainerRequestContext;
import jakarta.ws.rs.container.ContainerRequestFilter;
import jakarta.ws.rs.container.PreMatching;
import jakarta.ws.rs.core.Context;
import li.strolch.rest.helper.RestfulHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;

import static li.strolch.rest.StrolchRestfulConstants.STROLCH_REMOTE_IP;
import static li.strolch.rest.StrolchRestfulConstants.STROLCH_REQUEST_URL;

@PreMatching
public class LogRequestFilter implements ContainerRequestFilter {

	private static final Logger logger = LoggerFactory.getLogger(LogRequestFilter.class);

	@Context
	private HttpServletRequest request;

	@Override
	public void filter(ContainerRequestContext requestContext) throws IOException {
		String remoteIp = RestfulHelper.getRemoteIp(this.request);
		logger.info("Remote IP: {}: {} {}", remoteIp, requestContext.getMethod(),
				requestContext.getUriInfo().getRequestUri());

		this.request.setAttribute(STROLCH_REMOTE_IP, remoteIp);
		this.request.setAttribute(STROLCH_REQUEST_URL,
				requestContext.getMethod() + " " + requestContext.getUriInfo().getRequestUri());
	}
}
