/**
 * 
 */
package li.strolch.rest.filters;

import static li.strolch.rest.StrolchRestfulConstants.STROLCH_CERTIFICATE;

import java.io.IOException;

import javax.ws.rs.container.ContainerRequestContext;
import javax.ws.rs.container.ContainerResponseContext;
import javax.ws.rs.container.ContainerResponseFilter;
import javax.ws.rs.core.HttpHeaders;
import javax.ws.rs.ext.Provider;

import ch.eitchnet.privilege.model.Certificate;

/**
 * @author Reto Breitenmoser <reto.breitenmoser@4trees.ch>
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@Provider
public class AuthenicationResponseFilter implements ContainerResponseFilter {

	@Override
	public void filter(ContainerRequestContext requestContext, ContainerResponseContext responseContext)
			throws IOException {

		Certificate cert = (Certificate) requestContext.getProperty(STROLCH_CERTIFICATE);
		if (cert != null) {
			responseContext.getHeaders().add(HttpHeaders.AUTHORIZATION, cert.getAuthToken());
		}
	}
}
