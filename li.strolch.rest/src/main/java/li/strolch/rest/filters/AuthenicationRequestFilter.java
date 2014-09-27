/**
 * 
 */
package li.strolch.rest.filters;

import static li.strolch.rest.StrolchRestfulConstants.STROLCH_CERTIFICATE;

import java.io.IOException;

import javax.ws.rs.container.ContainerRequestContext;
import javax.ws.rs.container.ContainerRequestFilter;
import javax.ws.rs.core.HttpHeaders;
import javax.ws.rs.core.Response;
import javax.ws.rs.ext.Provider;

import li.strolch.rest.RestfulStrolchComponent;
import li.strolch.rest.StrolchSessionHandler;
import ch.eitchnet.privilege.model.Certificate;

/**
 * @author Reto Breitenmoser <reto.breitenmoser@4trees.ch>
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@Provider
public class AuthenicationRequestFilter implements ContainerRequestFilter {

	@Override
	public void filter(ContainerRequestContext requestContext) throws IOException {
		String sessionId = requestContext.getHeaderString(HttpHeaders.AUTHORIZATION);
		if (sessionId != null) {
			try {
				StrolchSessionHandler sessionHandler = RestfulStrolchComponent.getInstance().getComponent(
						StrolchSessionHandler.class);
				Certificate certificate = sessionHandler.validate(sessionId);
				requestContext.setProperty(STROLCH_CERTIFICATE, certificate);
			} catch (Exception e) {
				requestContext.abortWith(Response.status(Response.Status.UNAUTHORIZED)
						.entity("User cannot access the resource.").build()); //$NON-NLS-1$
			}
		}
	}
}
