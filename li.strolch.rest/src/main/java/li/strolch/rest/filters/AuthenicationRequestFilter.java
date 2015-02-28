/**
 * 
 */
package li.strolch.rest.filters;

import static li.strolch.rest.StrolchRestfulConstants.STROLCH_CERTIFICATE;

import java.io.IOException;
import java.util.List;

import javax.ws.rs.container.ContainerRequestContext;
import javax.ws.rs.container.ContainerRequestFilter;
import javax.ws.rs.core.HttpHeaders;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.ext.Provider;

import li.strolch.rest.RestfulStrolchComponent;
import li.strolch.rest.StrolchSessionHandler;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ch.eitchnet.privilege.model.Certificate;
import ch.eitchnet.utils.helper.StringHelper;

/**
 * @author Reto Breitenmoser <reto.breitenmoser@4trees.ch>
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@Provider
public class AuthenicationRequestFilter implements ContainerRequestFilter {

	private static final Logger logger = LoggerFactory.getLogger(AuthenicationRequestFilter.class);

	@Override
	public void filter(ContainerRequestContext requestContext) throws IOException {

		List<String> matchedURIs = requestContext.getUriInfo().getMatchedURIs();

		// we allow unauthorized access to the authentication service
		if (matchedURIs.contains("strolch/authentication")) {
			return;
		}

		String sessionId = requestContext.getHeaderString(HttpHeaders.AUTHORIZATION);
		if (StringHelper.isEmpty(sessionId)) {
			logger.error("No SessionID on request to URL " + requestContext.getUriInfo().getPath());
			requestContext.abortWith(Response.status(Response.Status.UNAUTHORIZED)
					.header(HttpHeaders.CONTENT_TYPE, MediaType.TEXT_PLAIN).entity("Missing Authorization!").build()); //$NON-NLS-1$
			return;
		}

		try {
			StrolchSessionHandler sessionHandler = RestfulStrolchComponent.getInstance().getComponent(
					StrolchSessionHandler.class);
			Certificate certificate = sessionHandler.validate(sessionId);
			requestContext.setProperty(STROLCH_CERTIFICATE, certificate);
		} catch (Exception e) {
			logger.error(e.getMessage(), e);
			requestContext.abortWith(Response.status(Response.Status.UNAUTHORIZED)
					.header(HttpHeaders.CONTENT_TYPE, MediaType.TEXT_PLAIN)
					.entity("User cannot access the resource.").build()); //$NON-NLS-1$
		}
	}
}
