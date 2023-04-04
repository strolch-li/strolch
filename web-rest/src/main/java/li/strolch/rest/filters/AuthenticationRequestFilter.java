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

import static li.strolch.rest.StrolchRestfulConstants.*;
import static li.strolch.utils.helper.StringHelper.*;

import jakarta.annotation.Priority;
import jakarta.servlet.http.HttpServletRequest;

import jakarta.ws.rs.Priorities;
import jakarta.ws.rs.container.ContainerRequestContext;
import jakarta.ws.rs.container.ContainerRequestFilter;
import jakarta.ws.rs.core.*;
import jakarta.ws.rs.ext.Provider;

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
 * <p>
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

	protected RestfulStrolchComponent getRestful() {
		return RestfulStrolchComponent.getInstance();
	}

	protected StrolchSessionHandler getSessionHandler() {
		return getRestful().getSessionHandler();
	}

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

		try {

			if (isUnsecuredPath(requestContext)) {
				setCertificateIfAvailable(requestContext, remoteIp);
			} else {
				validateSession(requestContext, remoteIp);
			}

		} catch (StrolchNotAuthenticatedException e) {
			logger.error(e.getMessage());
			requestContext.abortWith(Response.status(Response.Status.UNAUTHORIZED)
					.header(HttpHeaders.CONTENT_TYPE, MediaType.TEXT_PLAIN)
					.entity("User is not authenticated!")
					.build());
		} catch (StrolchAccessDeniedException e) {
			logger.error(e.getMessage());
			requestContext.abortWith(Response.status(Response.Status.FORBIDDEN)
					.header(HttpHeaders.CONTENT_TYPE, MediaType.TEXT_PLAIN)
					.entity("User is not authorized!")
					.build());
		} catch (Exception e) {
			logger.error(e.getMessage());
			requestContext.abortWith(Response.status(Response.Status.FORBIDDEN)
					.header(HttpHeaders.CONTENT_TYPE, MediaType.TEXT_PLAIN)
					.entity("User cannot access the resource.")
					.build());
		}
	}

	protected void setCertificateIfAvailable(ContainerRequestContext requestContext, String remoteIp) {
		StrolchSessionHandler sessionHandler = getSessionHandler();

		String sessionId = trimOrEmpty(requestContext.getHeaderString(HttpHeaders.AUTHORIZATION));
		if (isNotEmpty(sessionId)) {
			if (sessionHandler.isSessionKnown(sessionId)) {
				validateCertificate(requestContext, sessionId, remoteIp);
			} else {
				logger.error("Session " + sessionId + " by authorization header does not exist anymore, ignoring!");
			}

			return;
		}

		sessionId = getSessionIdFromCookie(requestContext);
		if (isEmpty(sessionId)) {
			return;
		}

		if (sessionHandler.isSessionKnown(sessionId)) {
			validateCertificate(requestContext, sessionId, remoteIp);
		} else {
			logger.error("Session " + sessionId + " by cookie does not exist anymore, ignoring!");
		}
	}

	/**
	 * Validate the given request context by checking for the authorization cookie or header and then verifying a
	 * session exists and is valid with the given authoriation token
	 *
	 * <br>
	 * <p>
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
		String authorization = trimOrEmpty(requestContext.getHeaderString(HttpHeaders.AUTHORIZATION));
		if (authorization.isEmpty())
			return validateCookie(requestContext, remoteIp);
		if (authorization.startsWith("Basic "))
			return authenticateBasic(requestContext, authorization, remoteIp);
		return validateCertificate(requestContext, authorization, remoteIp);
	}

	protected String getSessionIdFromCookie(ContainerRequestContext requestContext) {
		Cookie cookie = requestContext.getCookies().get(STROLCH_AUTHORIZATION);
		if (cookie == null)
			return "";

		String sessionId = cookie.getValue();
		if (sessionId == null)
			return "";

		return sessionId.trim();
	}

	protected Certificate validateCookie(ContainerRequestContext requestContext, String remoteIp) {
		String sessionId = getSessionIdFromCookie(requestContext);
		if (isEmpty(sessionId)) {
			logger.error(
					"No Authorization header or cookie on request to URL " + requestContext.getUriInfo().getPath());
			requestContext.abortWith(Response.status(Response.Status.FORBIDDEN)
					.header(HttpHeaders.CONTENT_TYPE, MediaType.TEXT_PLAIN)
					.entity("Missing Authorization!")
					.build());
			return null;
		}

		return validateCertificate(requestContext, sessionId, remoteIp);
	}

	protected Certificate authenticateBasic(ContainerRequestContext requestContext, String authorization,
			String remoteIp) {

		if (!getRestful().isBasicAuthEnabled()) {
			logger.error("Basic Auth is not available for URL " + requestContext.getUriInfo().getPath());
			requestContext.abortWith(Response.status(Response.Status.FORBIDDEN)
					.header(HttpHeaders.CONTENT_TYPE, MediaType.TEXT_PLAIN)
					.entity("Basic Auth not available")
					.build());
			return null;
		}

		String basicAuth = authorization.substring("Basic ".length());
		basicAuth = new String(Base64.getDecoder().decode(basicAuth.getBytes()), StandardCharsets.UTF_8);
		String[] parts = basicAuth.split(":");
		if (parts.length != 2) {
			requestContext.abortWith(Response.status(Response.Status.BAD_REQUEST)
					.header(HttpHeaders.CONTENT_TYPE, MediaType.TEXT_PLAIN)
					.entity("Invalid Basic Authorization!")
					.build());
			return null;
		}

		logger.info("Performing basic auth for user " + parts[0] + "...");
		StrolchSessionHandler sessionHandler = getSessionHandler();
		Certificate certificate = sessionHandler.authenticate(parts[0], parts[1].toCharArray(), remoteIp, Usage.SINGLE,
				false);

		requestContext.setProperty(STROLCH_CERTIFICATE, certificate);
		requestContext.setProperty(STROLCH_REQUEST_SOURCE, remoteIp);

		return certificate;
	}

	protected Certificate validateCertificate(ContainerRequestContext requestContext, String sessionId,
			String remoteIp) {
		StrolchSessionHandler sessionHandler = getSessionHandler();
		Certificate certificate = sessionHandler.validate(sessionId, remoteIp);

		if (certificate.getUsage() == Usage.SET_PASSWORD) {
			if (!requestContext.getUriInfo()
					.getMatchedURIs()
					.contains("strolch/privilege/users/" + certificate.getUsername() + "/password")) {
				requestContext.abortWith(Response.status(Response.Status.FORBIDDEN)
						.header(HttpHeaders.CONTENT_TYPE, MediaType.TEXT_PLAIN)
						.entity("Can only set password!")
						.build());
				return null;
			}
		}

		requestContext.setProperty(STROLCH_CERTIFICATE, certificate);
		requestContext.setProperty(STROLCH_REQUEST_SOURCE, remoteIp);
		return certificate;
	}

	public static String getRemoteIp(HttpServletRequest request) {
		if (request == null) {
			logger.error("HttpServletRequest NOT AVAILABLE! Probably running in TEST!");
			return "(null)";
		}

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
