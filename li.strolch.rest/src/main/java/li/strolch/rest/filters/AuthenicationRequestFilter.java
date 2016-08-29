/*
 * Copyright 2015 Robert von Burg <eitch@eitchnet.ch>
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *     http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package li.strolch.rest.filters;

import static li.strolch.rest.StrolchRestfulConstants.STROLCH_CERTIFICATE;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import javax.ws.rs.container.ContainerRequestContext;
import javax.ws.rs.container.ContainerRequestFilter;
import javax.ws.rs.core.Cookie;
import javax.ws.rs.core.HttpHeaders;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.ext.Provider;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import li.strolch.privilege.model.Certificate;
import li.strolch.rest.RestfulStrolchComponent;
import li.strolch.rest.StrolchRestfulConstants;
import li.strolch.rest.StrolchSessionHandler;
import li.strolch.utils.helper.StringHelper;

/**
 * @author Reto Breitenmoser <reto.breitenmoser@4trees.ch>
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@Provider
public class AuthenicationRequestFilter implements ContainerRequestFilter {

	private static final Logger logger = LoggerFactory.getLogger(AuthenicationRequestFilter.class);

	protected List<String> getUnsecuredPaths() {
		List<String> list = new ArrayList<>();
		list.add("strolch/authentication");
		return list;
	}

	@Override
	public void filter(ContainerRequestContext requestContext) throws IOException {

		List<String> matchedURIs = requestContext.getUriInfo().getMatchedURIs();

		// we allow unauthorized access to the authentication service
		if (matchedURIs.stream().anyMatch(s -> getUnsecuredPaths().contains(s))) {
			return;
		}

		String sessionId = requestContext.getHeaderString(HttpHeaders.AUTHORIZATION);
		if (StringHelper.isEmpty(sessionId)) {

			Cookie cookie = requestContext.getCookies().get(StrolchRestfulConstants.STROLCH_AUTHORIZATION);
			if (cookie == null) {
				logger.error(
						"No Authorization header or cookie on request to URL " + requestContext.getUriInfo().getPath());
				requestContext.abortWith(Response.status(Response.Status.FORBIDDEN)
						.header(HttpHeaders.CONTENT_TYPE, MediaType.TEXT_PLAIN).entity("Missing Authorization!") //$NON-NLS-1$
						.build());
				return;
			}

			sessionId = cookie.getValue();
			if (StringHelper.isEmpty(sessionId)) {
				logger.error("Authorization Cookie value missing on request to URL "
						+ requestContext.getUriInfo().getPath());
				requestContext.abortWith(Response.status(Response.Status.FORBIDDEN)
						.header(HttpHeaders.CONTENT_TYPE, MediaType.TEXT_PLAIN).entity("Missing Authorization!") //$NON-NLS-1$
						.build());
				return;
			}
		}

		try {
			StrolchSessionHandler sessionHandler = RestfulStrolchComponent.getInstance()
					.getComponent(StrolchSessionHandler.class);
			Certificate certificate = sessionHandler.validate(sessionId);
			requestContext.setProperty(STROLCH_CERTIFICATE, certificate);
		} catch (Exception e) {
			logger.error(e.getMessage(), e);
			requestContext.abortWith(
					Response.status(Response.Status.FORBIDDEN).header(HttpHeaders.CONTENT_TYPE, MediaType.TEXT_PLAIN)
							.entity("User cannot access the resource.").build()); //$NON-NLS-1$
		}
	}
}
