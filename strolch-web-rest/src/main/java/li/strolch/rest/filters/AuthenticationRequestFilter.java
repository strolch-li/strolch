/*
 * Copyright (c) 2015-2025 Robert von Burg <eitch@eitchnet.ch>
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

import com.google.gson.JsonObject;
import jakarta.annotation.Priority;
import jakarta.ws.rs.Priorities;
import jakarta.ws.rs.container.ContainerRequestContext;
import jakarta.ws.rs.container.ContainerRequestFilter;
import jakarta.ws.rs.core.*;
import jakarta.ws.rs.ext.Provider;
import li.strolch.agent.api.RealmHandler;
import li.strolch.agent.api.StrolchAgent;
import li.strolch.agent.api.StrolchRealm;
import li.strolch.exception.StrolchAccessDeniedException;
import li.strolch.exception.StrolchNotAuthenticatedException;
import li.strolch.handler.audits.AuditHandler;
import li.strolch.privilege.base.AccessDeniedException;
import li.strolch.privilege.base.InvalidCredentialsException;
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

import static jakarta.ws.rs.core.HttpHeaders.*;
import static java.lang.String.join;
import static li.strolch.model.Tags.Json.*;
import static li.strolch.rest.StrolchRestfulConstants.*;
import static li.strolch.utils.helper.StringHelper.*;
import static org.glassfish.jersey.http.HttpHeaders.*;

/**
 * This authentication request filter secures any requests to a Strolch server, by verifying that the request contains
 * either the cookie {@link StrolchRestfulConstants#STROLCH_AUTHORIZATION} containing the authorization token, or the
 * header {@link HttpHeaders#AUTHORIZATION} with the authorization token as its value.
 *
 * <br>
 * <p>
 * Sub classes should override {@link #validateSession(ContainerRequestContext, String)} to add further validation.
 *
 * @author Reto Breitenmoser <reto.breitenmoser@atexxi.ch>
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

			certificate.ifPresent(cert -> {
				CertificateThreadLocal.setCert(cert);
				writeAudit(requestContext, cert, remoteIp);
			});

		} catch (StrolchNotAuthenticatedException | InvalidCredentialsException e) {
			logger.error(e.getMessage());
			requestContext.abortWith(Response
					.status(Response.Status.UNAUTHORIZED)
					.header(CONTENT_TYPE, MediaType.TEXT_PLAIN)
					.entity("User is not authenticated!")
					.build());
		} catch (StrolchAccessDeniedException | AccessDeniedException e) {
			logger.error(e.getMessage());
			requestContext.abortWith(Response
					.status(Response.Status.UNAUTHORIZED)
					.header(CONTENT_TYPE, MediaType.TEXT_PLAIN)
					.entity("User is not authorized!")
					.build());
		} catch (Exception e) {
			logger.error(e.getMessage(), e);
			requestContext.abortWith(Response
					.status(Response.Status.INTERNAL_SERVER_ERROR)
					.header(CONTENT_TYPE, MediaType.TEXT_PLAIN)
					.entity("User cannot access the resource.")
					.build());
		}
	}

	private static final Set<String> skippedHeaders = Set.of(AUTHORIZATION.toLowerCase(), USER_AGENT.toLowerCase(),
			HOST.toLowerCase(), ACCEPT_ENCODING.toLowerCase(), ACCEPT_LANGUAGE.toLowerCase(), COOKIE.toLowerCase(),
			ORIGIN.toLowerCase(), REFERER.toLowerCase(), CONTENT_LENGTH.toLowerCase(), CACHE_CONTROL.toLowerCase(),
			CONNECTION.toLowerCase(), "upgrade-insecure-requests", "dnt", "priority");

	private static void writeAudit(ContainerRequestContext requestContext, Certificate cert, String remoteIp) {
		StrolchAgent agent = RestfulStrolchComponent.getInstance().getAgent();
		StrolchRealm realm = agent.getComponent(RealmHandler.class).getRealm(cert.getRealmOrDefault());
		if (!realm.isAuditTrailEnabled())
			return;

		String _url = requestContext.getUriInfo().getPath();

		if (_url.startsWith("strolch/authentication/")) {
			String extractedUrl = _url.substring("strolch/authentication/".length());

			// Check if URL ends with an SHA-256 checksum
			if (extractedUrl.length() == 64 && extractedUrl.matches("[a-fA-F0-9]{64}"))
				_url = "strolch/authentication/xxx";
		}

		String url = _url;

		String method = requestContext.getMethod();

		JsonObject headers = new JsonObject();
		requestContext.getHeaders().forEach((key, value) -> {
			if (skippedHeaders.contains(key.toLowerCase()))
				return;
			if (key.toLowerCase().startsWith("sec-"))
				return;
			if (key.equalsIgnoreCase(AUTHORIZATION))
				headers.addProperty(key, "***");
			else
				headers.addProperty(key, join(", ", value));
		});
		MultivaluedMap<String, String> queryParams = requestContext.getUriInfo().getQueryParameters();
		JsonObject params = new JsonObject();
		queryParams.forEach((key, value) -> params.addProperty(key, join(", ", value)));

		JsonObject additionalData = new JsonObject();
		additionalData.addProperty(METHOD, method);
		additionalData.addProperty(URL, url);
		additionalData.addProperty(REMOTE_IP, remoteIp);
		if (!headers.isEmpty())
			additionalData.add(HEADERS, headers);
		if (!params.isEmpty())
			additionalData.add(PARAMS, params);

		agent
				.getComponentO(AuditHandler.class)
				.ifPresent(handler -> handler.writeAuditForApiCallAsync(cert, url, method, additionalData));
	}

	protected Optional<Certificate> setCertificateIfAvailable(ContainerRequestContext requestContext, String remoteIp) {
		StrolchSessionHandler sessionHandler = getSessionHandler();

		String sessionId = trimOrEmpty(requestContext.getHeaderString(AUTHORIZATION));
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
		String authorization = trimOrEmpty(requestContext.getHeaderString(AUTHORIZATION));
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
					.header(CONTENT_TYPE, MediaType.TEXT_PLAIN)
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
					.header(CONTENT_TYPE, MediaType.TEXT_PLAIN)
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
					.header(CONTENT_TYPE, MediaType.TEXT_PLAIN)
					.entity("Invalid Basic Authorization!")
					.build());
			return Optional.empty();
		}

		String username = parts[0];
		String password = parts[1];
		logger.debug("Performing basic auth for user {}...", username);
		StrolchSessionHandler sessionHandler = getSessionHandler();
		Certificate certificate = sessionHandler.authenticate(username, password.toCharArray(), remoteIp, Usage.SINGLE,
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
					.header(CONTENT_TYPE, MediaType.TEXT_PLAIN)
					.entity("User is not authenticated!")
					.build());
			return Optional.empty();
		}

		Certificate certificate = sessionHandler.validate(sessionId, remoteIp);

		if (certificate.getUsage() == Usage.SET_PASSWORD) {
			String allowedPwUrl = ("strolch/privilege/users/" + certificate.getUserId() + "/password").toLowerCase();
			if (requestContext
					.getUriInfo()
					.getMatchedURIs()
					.stream()
					.noneMatch(s -> s.toLowerCase().equals(allowedPwUrl))) {
				requestContext.abortWith(Response
						.status(Response.Status.FORBIDDEN)
						.header(CONTENT_TYPE, MediaType.TEXT_PLAIN)
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
