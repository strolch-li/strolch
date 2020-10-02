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
import static li.strolch.rest.StrolchRestfulConstants.STROLCH_REQUEST_SOURCE;
import static li.strolch.utils.helper.StringHelper.isEmpty;
import static li.strolch.utils.helper.StringHelper.isNotEmpty;

import javax.annotation.Priority;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.Priorities;
import javax.ws.rs.container.ContainerRequestContext;
import javax.ws.rs.container.ContainerRequestFilter;
import javax.ws.rs.core.*;
import javax.ws.rs.ext.Provider;
import java.nio.charset.StandardCharsets;
import java.util.Base64;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import li.strolch.exception.StrolchAccessDeniedException;
import li.strolch.exception.StrolchNotAuthenticatedException;
import li.strolch.privilege.model.Certificate;
import li.strolch.privilege.model.Usage;
import li.strolch.rest.RestfulStrolchComponent;
import li.strolch.rest.StrolchRestfulConstants;
import li.strolch.rest.StrolchSessionHandler;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This authentication request filter secures any requests to a Strolch server, by verifying that the request contains
 * either the cookie {@link StrolchRestfulConstants#STROLCH_AUTHORIZATION} containing the authorization token, or the
 * header {@link HttpHeaders#AUTHORIZATION} with the authorization token as its value.
 *
 * <br>
 *
 * Sub classes should override {@link #validateSession(ContainerRequestContext, String)} to add further validation.
 *
 * @author Reto Breitenmoser <reto.breitenmoser@4trees.ch>
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@Provider
@Priority(Priorities.AUTHENTICATION)
public class AuthenticationRequestFilter implements ContainerRequestFilter {

	private static final Logger logger = LoggerFactory.getLogger(AuthenticationRequestFilter.class);

	@Context
	private HttpServletRequest request;

	private Set<String> unsecuredPaths;

	/**
	 * Defines the set of paths which are considered to be unsecured, i.e. can be requested without having logged in
	 * prior to the request
	 *
	 * @return the set of unsecured paths
	 */
	protected Set<String> getUnsecuredPaths() {
		Set<String> paths = new HashSet<>();
		paths.add("strolch/authentication");
		paths.add("strolch/authentication/sso");
		paths.add("strolch/version");
		return paths;
	}

	/**
	 * Validates if the path for the given request is for an unsecured path, i.e. no authorization is required
	 *
	 * @param requestContext
	 * 		the request context
	 *
	 * @return true if the request context is for an unsecured path, false if not, meaning authorization must be
	 * validated
	 */
	protected boolean isUnsecuredPath(ContainerRequestContext requestContext) {

		// we have to allow OPTIONS for CORS
		if (requestContext.getMethod().equals("OPTIONS"))
			return true;

		List<String> matchedURIs = requestContext.getUriInfo().getMatchedURIs();

		// we allow unauthorized access to the authentication service
		if (this.unsecuredPaths == null)
			this.unsecuredPaths = getUnsecuredPaths();

		return matchedURIs.stream().anyMatch(s -> this.unsecuredPaths.contains(s));
	}

	@Override
	public void filter(ContainerRequestContext requestContext) {
		String remoteIp = getRemoteIp(this.request);
		logger.info("Remote IP: " + remoteIp + ": " + requestContext.getMethod() + " " + requestContext.getUriInfo()
				.getRequestUri());

		if (isUnsecuredPath(requestContext))
			return;

		try {

			validateSession(requestContext, remoteIp);

		} catch (StrolchNotAuthenticatedException e) {
			logger.error(e.getMessage());
			requestContext.abortWith(
					Response.status(Response.Status.UNAUTHORIZED).header(HttpHeaders.CONTENT_TYPE, MediaType.TEXT_PLAIN)
							.entity("User is not authenticated!").build()); //$NON-NLS-1$
		} catch (StrolchAccessDeniedException e) {
			logger.error(e.getMessage());
			requestContext.abortWith(
					Response.status(Response.Status.FORBIDDEN).header(HttpHeaders.CONTENT_TYPE, MediaType.TEXT_PLAIN)
							.entity("User is not authorized!").build()); //$NON-NLS-1$
		} catch (Exception e) {
			logger.error(e.getMessage());
			requestContext.abortWith(
					Response.status(Response.Status.FORBIDDEN).header(HttpHeaders.CONTENT_TYPE, MediaType.TEXT_PLAIN)
							.entity("User cannot access the resource.").build()); //$NON-NLS-1$
		}
	}

	/**
	 * Validate the given request context by checking for the authorization cookie or header and then verifying a
	 * session exists and is valid with the given authoriation token
	 *
	 * <br>
	 *
	 * Sub classes should override this method and first call super. If the return value is non-null, then further
	 * validation can be performed
	 *
	 * @param requestContext
	 * 		the request context for the secured path
	 * @param remoteIp
	 * 		the remote IP
	 *
	 * @return the certificate for the validated session, or null, of the request is aborted to no missing or invalid
	 * authorization token
	 */
	protected Certificate validateSession(ContainerRequestContext requestContext, String remoteIp) {

		String authorization = requestContext.getHeaderString(HttpHeaders.AUTHORIZATION);
		authorization = authorization == null ? "" : authorization.trim();

		if (isEmpty(authorization) || (authorization.startsWith("Basic ") && !getRestful().isBasicAuthEnabled())) {
			return validateCookie(requestContext, remoteIp);
		}

		boolean basicAuth = authorization.startsWith("Basic ");
		if (basicAuth) {

			// do basic auth, if enabled
			if (getRestful().isBasicAuthEnabled())
				return authenticateBasic(requestContext, authorization, remoteIp);

			// otherwise see if we can do cookie auth
			String sessionId = getSessionIdFromCookie(requestContext);
			if (!sessionId.isEmpty())
				return validateCertificate(requestContext, sessionId, remoteIp);

			logger.error("Basic Auth not enabled. Can not process URL " + requestContext.getUriInfo().getPath());
			requestContext.abortWith(
					Response.status(Response.Status.FORBIDDEN).header(HttpHeaders.CONTENT_TYPE, MediaType.TEXT_PLAIN)
							.entity("Basic Auth not enabled") //$NON-NLS-1$
							.build());
			return null;
		}

		return validateCertificate(requestContext, authorization, remoteIp);
	}

	private String getSessionIdFromCookie(ContainerRequestContext requestContext) {
		Cookie cookie = requestContext.getCookies().get(StrolchRestfulConstants.STROLCH_AUTHORIZATION);
		if (cookie == null)
			return "";

		String sessionId = cookie.getValue();
		if (sessionId == null)
			return "";

		return sessionId.trim();
	}

	private Certificate validateCookie(ContainerRequestContext requestContext, String remoteIp) {
		String sessionId = getSessionIdFromCookie(requestContext);
		if (isEmpty(sessionId)) {
			logger.error(
					"No Authorization header or cookie on request to URL " + requestContext.getUriInfo().getPath());
			requestContext.abortWith(
					Response.status(Response.Status.FORBIDDEN).header(HttpHeaders.CONTENT_TYPE, MediaType.TEXT_PLAIN)
							.entity("Missing Authorization!") //$NON-NLS-1$
							.build());
			return null;
		}

		return validateCertificate(requestContext, sessionId, remoteIp);
	}

	private RestfulStrolchComponent getRestful() {
		return RestfulStrolchComponent.getInstance();
	}

	private Certificate authenticateBasic(ContainerRequestContext requestContext, String authorization,
			String remoteIp) {
		String basicAuth = authorization.substring("Basic ".length());
		basicAuth = new String(Base64.getDecoder().decode(basicAuth.getBytes()), StandardCharsets.UTF_8);
		String[] parts = basicAuth.split(":");
		if (parts.length != 2) {
			requestContext.abortWith(
					Response.status(Response.Status.BAD_REQUEST).header(HttpHeaders.CONTENT_TYPE, MediaType.TEXT_PLAIN)
							.entity("Invalid Basic Authorization!") //$NON-NLS-1$
							.build());
			return null;
		}

		logger.info("Performing basic auth for user " + parts[0] + "...");
		StrolchSessionHandler sessionHandler = getSessionHandler();
		Certificate certificate = sessionHandler
				.authenticate(parts[0], parts[1].toCharArray(), remoteIp, Usage.SINGLE, false);

		requestContext.setProperty(STROLCH_CERTIFICATE, certificate);
		requestContext.setProperty(STROLCH_REQUEST_SOURCE, remoteIp);

		return certificate;
	}

	private StrolchSessionHandler getSessionHandler() {
		return getRestful().getSessionHandler();
	}

	private Certificate validateCertificate(ContainerRequestContext requestContext, String sessionId, String remoteIp) {
		StrolchSessionHandler sessionHandler = getSessionHandler();
		Certificate certificate = sessionHandler.validate(sessionId, remoteIp);

		requestContext.setProperty(STROLCH_CERTIFICATE, certificate);
		requestContext.setProperty(STROLCH_REQUEST_SOURCE, remoteIp);
		return certificate;
	}

	public static String getRemoteIp(HttpServletRequest request) {

		String remoteHost = request.getRemoteHost();
		String remoteAddr = request.getRemoteAddr();

		StringBuilder sb = new StringBuilder();
		if (remoteHost.equals(remoteAddr))
			sb.append(remoteAddr);
		else {
			sb.append(remoteHost).append(": (").append(remoteAddr).append(")");
		}

		String xForwardedFor = request.getHeader("X-Forwarded-For");
		if (isNotEmpty(xForwardedFor))
			sb.append(" (fwd)=> ").append(xForwardedFor);

		return sb.toString();
	}
}
