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

import jakarta.annotation.Priority;
import jakarta.ws.rs.Priorities;
import jakarta.ws.rs.container.ContainerRequestContext;
import jakarta.ws.rs.container.ContainerRequestFilter;
import jakarta.ws.rs.core.Cookie;
import jakarta.ws.rs.core.HttpHeaders;
import jakarta.ws.rs.core.MediaType;
import jakarta.ws.rs.core.Response;
import jakarta.ws.rs.ext.Provider;
import li.strolch.exception.StrolchAccessDeniedException;
import li.strolch.exception.StrolchNotAuthenticatedException;
import li.strolch.privilege.model.Certificate;
import li.strolch.privilege.model.CertificateThreadLocal;
import li.strolch.privilege.model.Usage;
import li.strolch.rest.RestfulStrolchComponent;
import li.strolch.rest.StrolchRestfulConstants;
import li.strolch.runtime.sessions.StrolchSessionHandler;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.nio.charset.StandardCharsets;
import java.util.*;

import static li.strolch.rest.StrolchRestfulConstants.*;
import static li.strolch.utils.helper.StringHelper.*;

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
		paths.add("strolch/languages");
		return paths;
	}

	/**
	 * Validates if the path for the given request is for an unsecured path, i.e. no authorization is required
	 *
	 * @param requestContext the request context
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

		String remoteIp = (String) requestContext.getProperty(STROLCH_REMOTE_IP);

		try {

			Optional<Certificate> certificate;
			if (isUnsecuredPath(requestContext)) {
				certificate = setCertificateIfAvailable(requestContext, remoteIp);
			} else {
				certificate = validateSession(requestContext, remoteIp);
			}

			certificate.ifPresent(CertificateThreadLocal::setCert);

		} catch (StrolchNotAuthenticatedException e) {
			logger.error(e.getMessage());
			requestContext.abortWith(Response
					.status(Response.Status.UNAUTHORIZED)
					.header(HttpHeaders.CONTENT_TYPE, MediaType.TEXT_PLAIN)
					.entity("User is not authenticated!")
					.build());
		} catch (StrolchAccessDeniedException e) {
			logger.error(e.getMessage());
			requestContext.abortWith(Response
					.status(Response.Status.UNAUTHORIZED)
					.header(HttpHeaders.CONTENT_TYPE, MediaType.TEXT_PLAIN)
					.entity("User is not authorized!")
					.build());
		} catch (Exception e) {
			logger.error(e.getMessage());
			requestContext.abortWith(Response
					.status(Response.Status.INTERNAL_SERVER_ERROR)
					.header(HttpHeaders.CONTENT_TYPE, MediaType.TEXT_PLAIN)
					.entity("User cannot access the resource.")
					.build());
		}
	}

	protected Optional<Certificate> setCertificateIfAvailable(ContainerRequestContext requestContext, String remoteIp) {
		StrolchSessionHandler sessionHandler = getSessionHandler();

		String sessionId = trimOrEmpty(requestContext.getHeaderString(HttpHeaders.AUTHORIZATION));
		if (isNotEmpty(sessionId)) {
			if (sessionHandler.isSessionKnown(sessionId)) {
				return validateCertificate(requestContext, sessionId, remoteIp);
			} else {
				logger.error("Session {} by authorization header does not exist anymore, ignoring!", sessionId);
				return Optional.empty();
			}
		}

		sessionId = getSessionIdFromCookie(requestContext);
		if (isEmpty(sessionId))
			return Optional.empty();

		if (sessionHandler.isSessionKnown(sessionId))
			return validateCertificate(requestContext, sessionId, remoteIp);

		logger.debug("Session {} by cookie does not exist anymore, ignoring!", sessionId);
		return Optional.empty();
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
	 * @param requestContext the request context for the secured path
	 * @param remoteIp       the remote IP
	 *
	 * @return the certificate for the validated session, or null, of the request is aborted to no missing or invalid
	 * authorization token
	 */
	protected Optional<Certificate> validateSession(ContainerRequestContext requestContext, String remoteIp) {
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

	protected Optional<Certificate> validateCookie(ContainerRequestContext requestContext, String remoteIp) {
		String sessionId = getSessionIdFromCookie(requestContext);
		if (isEmpty(sessionId)) {
			logger.error("No Authorization header or cookie on request to URL {}",
					requestContext.getUriInfo().getPath());
			requestContext.abortWith(Response
					.status(Response.Status.UNAUTHORIZED)
					.header(HttpHeaders.CONTENT_TYPE, MediaType.TEXT_PLAIN)
					.entity("Missing Authorization!")
					.build());
			return Optional.empty();
		}

		return validateCertificate(requestContext, sessionId, remoteIp);
	}

	protected Optional<Certificate> authenticateBasic(ContainerRequestContext requestContext, String authorization,
			String remoteIp) {

		if (!getRestful().isBasicAuthEnabled()) {
			logger.error("Basic Auth is not available for URL {}", requestContext.getUriInfo().getPath());
			requestContext.abortWith(Response
					.status(Response.Status.FORBIDDEN)
					.header(HttpHeaders.CONTENT_TYPE, MediaType.TEXT_PLAIN)
					.entity("Basic Auth not available")
					.build());
			return Optional.empty();
		}

		String basicAuth = authorization.substring("Basic ".length());
		basicAuth = new String(Base64.getDecoder().decode(basicAuth.getBytes()), StandardCharsets.UTF_8);
		String[] parts = basicAuth.split(":");
		if (parts.length != 2) {
			requestContext.abortWith(Response
					.status(Response.Status.BAD_REQUEST)
					.header(HttpHeaders.CONTENT_TYPE, MediaType.TEXT_PLAIN)
					.entity("Invalid Basic Authorization!")
					.build());
			return Optional.empty();
		}

		logger.debug("Performing basic auth for user {}...", parts[0]);
		StrolchSessionHandler sessionHandler = getSessionHandler();
		Certificate certificate = sessionHandler.authenticate(parts[0], parts[1].toCharArray(), remoteIp, Usage.SINGLE,
				false);

		requestContext.setProperty(STROLCH_CERTIFICATE, certificate);
		requestContext.setProperty(STROLCH_REQUEST_SOURCE, remoteIp);

		return Optional.ofNullable(certificate);
	}

	protected Optional<Certificate> validateCertificate(ContainerRequestContext requestContext, String sessionId,
			String remoteIp) {
		StrolchSessionHandler sessionHandler = getSessionHandler();
		if (!sessionHandler.isSessionKnown(sessionId)) {
			logger.debug("Ignoring unknown session!");
			requestContext.abortWith(Response
					.status(Response.Status.UNAUTHORIZED)
					.header(HttpHeaders.CONTENT_TYPE, MediaType.TEXT_PLAIN)
					.entity("User is not authenticated!")
					.build());
			return Optional.empty();
		}

		Certificate certificate = sessionHandler.validate(sessionId, remoteIp);

		if (certificate.getUsage() == Usage.SET_PASSWORD) {
			if (!requestContext
					.getUriInfo()
					.getMatchedURIs()
					.contains("strolch/privilege/users/" + certificate.getUsername() + "/password")) {
				requestContext.abortWith(Response
						.status(Response.Status.FORBIDDEN)
						.header(HttpHeaders.CONTENT_TYPE, MediaType.TEXT_PLAIN)
						.entity("Can only set password!")
						.build());
				return Optional.empty();
			}
		}

		requestContext.setProperty(STROLCH_CERTIFICATE, certificate);
		requestContext.setProperty(STROLCH_REQUEST_SOURCE, remoteIp);
		return Optional.of(certificate);
	}
}
