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
package li.strolch.rest.endpoint;

import static li.strolch.rest.StrolchRestfulConstants.STROLCH_AUTHORIZATION;
import static li.strolch.rest.StrolchRestfulConstants.STROLCH_AUTHORIZATION_EXPIRATION_DATE;
import static li.strolch.rest.filters.AuthenticationRequestFilter.getRemoteIp;

import java.text.MessageFormat;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.Base64;
import java.util.Date;
import java.util.Set;
import java.util.concurrent.TimeUnit;

import com.google.gson.*;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.ws.rs.*;
import jakarta.ws.rs.core.*;
import jakarta.ws.rs.core.Response.Status;
import li.strolch.exception.StrolchException;
import li.strolch.privilege.base.AccessDeniedException;
import li.strolch.privilege.base.InvalidCredentialsException;
import li.strolch.privilege.base.PrivilegeException;
import li.strolch.privilege.model.Certificate;
import li.strolch.privilege.model.IPrivilege;
import li.strolch.privilege.model.PrivilegeContext;
import li.strolch.privilege.model.Usage;
import li.strolch.rest.RestfulStrolchComponent;
import li.strolch.rest.StrolchSessionHandler;
import li.strolch.rest.helper.ResponseUtil;
import li.strolch.runtime.privilege.PrivilegeHandler;
import li.strolch.utils.helper.ExceptionHelper;
import li.strolch.utils.iso8601.ISO8601;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@Path("strolch/authentication")
public class AuthenticationService {

	private static final Logger logger = LoggerFactory.getLogger(AuthenticationService.class);

	@POST
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)
	public Response authenticate(@Context HttpServletRequest request, @Context HttpHeaders headers, String data) {

		JsonObject login = JsonParser.parseString(data).getAsJsonObject();

		try {

			if (!login.has("username") || login.get("username").getAsString().length() < 2) {
				logger.error("Authentication failed: Username was not given or is too short!");
				JsonObject loginResult = new JsonObject();
				loginResult.addProperty("msg", MessageFormat.format("Could not log in due to: {0}",
						"Username was not given or is too short!")); //$NON-NLS-2$
				return Response.status(Status.BAD_REQUEST).entity(loginResult.toString()).build();
			}

			if (!login.has("password") || login.get("password").getAsString().length() < 3) {
				logger.error("Authentication failed: Password was not given or is too short!");
				JsonObject loginResult = new JsonObject();
				loginResult.addProperty("msg", MessageFormat.format("Could not log in due to: {0}",
						"Password was not given or is too short!")); //$NON-NLS-2$
				return Response.status(Status.BAD_REQUEST).entity(loginResult.toString()).build();
			}

			String username = login.get("username").getAsString();
			String passwordEncoded = login.get("password").getAsString();
			boolean keepAlive = login.has("keepAlive") && login.get("keepAlive").getAsBoolean();

			byte[] decode = Base64.getDecoder().decode(passwordEncoded);
			char[] password = new String(decode).toCharArray();

			if (password.length < 3) {
				logger.error("Authentication failed: Password was not given or is too short!");
				JsonObject loginResult = new JsonObject();
				loginResult.addProperty("msg", MessageFormat.format("Could not log in due to: {0}",
						"Password was not given or is too short!")); //$NON-NLS-2$
				return Response.status(Status.BAD_REQUEST).entity(loginResult.toString()).build();
			}

			StrolchSessionHandler sessionHandler = RestfulStrolchComponent.getInstance().getSessionHandler();
			String source = getRemoteIp(request);
			Certificate certificate = sessionHandler.authenticate(username, password, source, Usage.ANY, keepAlive);

			return getAuthenticationResponse(request, certificate, source, true);

		} catch (InvalidCredentialsException e) {
			logger.error("Authentication failed due to: " + e.getMessage());
			JsonObject loginResult = new JsonObject();
			loginResult.addProperty("msg", "Could not log in as the given credentials are invalid");
			return Response.status(Status.UNAUTHORIZED).entity(loginResult.toString()).build();
		} catch (AccessDeniedException e) {
			logger.error("Authentication failed due to: " + e.getMessage());
			JsonObject loginResult = new JsonObject();
			loginResult.addProperty("msg",
					MessageFormat.format("Could not log in due to: {0}", e.getMessage())); //$NON-NLS-2$
			return Response.status(Status.UNAUTHORIZED).entity(loginResult.toString()).build();
		} catch (StrolchException | PrivilegeException e) {
			logger.error(e.getMessage(), e);
			JsonObject loginResult = new JsonObject();
			loginResult.addProperty("msg",
					MessageFormat.format("Could not log in due to: {0}", e.getMessage())); //$NON-NLS-2$
			return Response.status(Status.FORBIDDEN).entity(loginResult.toString()).build();
		} catch (Exception e) {
			logger.error(e.getMessage(), e);
			String msg = e.getMessage();
			JsonObject loginResult = new JsonObject();
			loginResult.addProperty("msg", MessageFormat.format("{0}: {1}", e.getClass().getName(), msg));
			return Response.serverError().entity(loginResult.toString()).build();
		}
	}

	@POST
	@Produces(MediaType.APPLICATION_JSON)
	@Path("sso")
	public Response authenticateSingleSignOn(@Context HttpServletRequest request, @Context HttpHeaders headers) {

		try {

			StrolchSessionHandler sessionHandler = RestfulStrolchComponent.getInstance().getSessionHandler();
			String source = getRemoteIp(request);
			Certificate certificate = sessionHandler.authenticateSingleSignOn(request.getUserPrincipal(), source);

			return getAuthenticationResponse(request, certificate, source, true);

		} catch (InvalidCredentialsException e) {
			logger.error("Authentication failed due to: " + e.getMessage());
			JsonObject loginResult = new JsonObject();
			loginResult.addProperty("msg", "Could not log in as the given credentials are invalid");
			return Response.status(Status.UNAUTHORIZED).entity(loginResult.toString()).build();
		} catch (AccessDeniedException e) {
			logger.error("Authentication failed due to: " + e.getMessage());
			JsonObject loginResult = new JsonObject();
			loginResult.addProperty("msg",
					MessageFormat.format("Could not log in due to: {0}", e.getMessage())); //$NON-NLS-2$
			return Response.status(Status.UNAUTHORIZED).entity(loginResult.toString()).build();
		} catch (StrolchException | PrivilegeException e) {
			logger.error(e.getMessage(), e);
			JsonObject loginResult = new JsonObject();
			loginResult.addProperty("msg",
					MessageFormat.format("Could not log in due to: {0}", e.getMessage())); //$NON-NLS-2$
			return Response.status(Status.FORBIDDEN).entity(loginResult.toString()).build();
		} catch (Exception e) {
			logger.error(e.getMessage(), e);
			String msg = e.getMessage();
			JsonObject loginResult = new JsonObject();
			loginResult.addProperty("msg", MessageFormat.format("{0}: {1}", e.getClass().getName(), msg));
			return Response.serverError().entity(loginResult.toString()).build();
		}
	}

	@DELETE
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{authToken}")
	public Response invalidateSession(@Context HttpServletRequest request, @PathParam("authToken") String authToken) {

		JsonObject logoutResult = new JsonObject();

		try {

			StrolchSessionHandler sessionHandler = RestfulStrolchComponent.getInstance().getSessionHandler();
			String source = getRemoteIp(request);
			Certificate certificate = sessionHandler.validate(authToken, source);
			sessionHandler.invalidate(certificate);

			logoutResult.addProperty("username", certificate.getUsername());
			logoutResult.addProperty("authToken", authToken);
			logoutResult.addProperty("msg",
					MessageFormat.format("{0} has been logged out.", certificate.getUsername()));
			return Response.ok().entity(logoutResult.toString()).build();

		} catch (StrolchException | PrivilegeException e) {
			logger.error("Failed to invalidate session due to: " + e.getMessage());
			logoutResult.addProperty("msg",
					MessageFormat.format("Could not logout due to: {0}", e.getMessage())); //$NON-NLS-2$
			return Response.status(Status.UNAUTHORIZED).entity(logoutResult.toString()).build();
		} catch (Exception e) {
			logger.error(e.getMessage(), e);
			String msg = e.getMessage();
			logoutResult.addProperty("msg", MessageFormat.format("{0}: {1}", e.getClass().getName(), msg));
			return Response.serverError().entity(logoutResult.toString()).build();
		}
	}

	@HEAD
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{authToken}")
	public Response validateSession(@Context HttpServletRequest request, @PathParam("authToken") String authToken) {

		try {

			StrolchSessionHandler sessionHandler = RestfulStrolchComponent.getInstance().getSessionHandler();
			String source = getRemoteIp(request);
			sessionHandler.validate(authToken, source);

			return Response.ok().build();

		} catch (StrolchException | PrivilegeException e) {
			logger.error("Session validation failed: " + e.getMessage());
			JsonObject root = new JsonObject();
			root.addProperty("msg", MessageFormat.format("Session invalid: {0}", e.getMessage()));
			String json = new Gson().toJson(root);
			return Response.status(Status.UNAUTHORIZED).entity(json).build();
		} catch (Exception e) {
			logger.error(e.getMessage(), e);
			String msg = e.getMessage();
			JsonObject root = new JsonObject();
			root.addProperty("msg", MessageFormat.format("Session invalid: {0}: {1}", e.getClass().getName(), msg));
			String json = new Gson().toJson(root);
			return Response.serverError().entity(json).build();
		}
	}

	@GET
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{authToken}")
	public Response getValidatedSession(@Context HttpServletRequest request, @PathParam("authToken") String authToken) {

		try {

			StrolchSessionHandler sessionHandler = RestfulStrolchComponent.getInstance().getSessionHandler();
			String source = getRemoteIp(request);
			Certificate certificate = sessionHandler.validate(authToken, source);

			return getAuthenticationResponse(request, certificate, source, false);

		} catch (StrolchException | PrivilegeException e) {
			logger.error("Session validation failed: " + e.getMessage());
			JsonObject root = new JsonObject();
			root.addProperty("msg", MessageFormat.format("Session invalid: {0}", e.getMessage()));
			String json = new Gson().toJson(root);
			return Response.status(Status.UNAUTHORIZED).entity(json).build();
		} catch (Exception e) {
			logger.error(e.getMessage(), e);
			String msg = e.getMessage();
			JsonObject root = new JsonObject();
			root.addProperty("msg", MessageFormat.format("Session invalid: {0}: {1}", e.getClass().getName(), msg));
			String json = new Gson().toJson(root);
			return Response.serverError().entity(json).build();
		}
	}

	@PUT
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{authToken}")
	public Response refreshSession(@Context HttpServletRequest request, @PathParam("authToken") String authToken) {

		try {

			StrolchSessionHandler sessionHandler = RestfulStrolchComponent.getInstance().getSessionHandler();
			String source = getRemoteIp(request);
			Certificate certificate = sessionHandler.validate(authToken, source);

			Certificate refreshedCert = sessionHandler.refreshSession(certificate, source);

			return getAuthenticationResponse(request, refreshedCert, source, true);

		} catch (StrolchException | PrivilegeException e) {
			logger.error("Session validation failed: " + e.getMessage());
			JsonObject root = new JsonObject();
			root.addProperty("msg", MessageFormat.format("Session invalid: {0}", e.getMessage()));
			String json = new Gson().toJson(root);
			return Response.status(Status.UNAUTHORIZED).entity(json).build();
		} catch (Exception e) {
			logger.error(e.getMessage(), e);
			String msg = e.getMessage();
			JsonObject root = new JsonObject();
			root.addProperty("msg", MessageFormat.format("Session invalid: {0}: {1}", e.getClass().getName(), msg));
			String json = new Gson().toJson(root);
			return Response.serverError().entity(json).build();
		}
	}

	@POST
	@Produces(MediaType.APPLICATION_JSON)
	@Path("challenge")
	public Response initiateChallenge(@Context HttpServletRequest request, String data) {

		try {
			JsonObject jsonObject = JsonParser.parseString(data).getAsJsonObject();
			String username = jsonObject.get("username").getAsString();
			String usage = jsonObject.get("usage").getAsString();

			StrolchSessionHandler sessionHandler = RestfulStrolchComponent.getInstance().getSessionHandler();
			String source = getRemoteIp(request);
			sessionHandler.initiateChallengeFor(Usage.byValue(usage), username, source);

			return ResponseUtil.toResponse();

		} catch (PrivilegeException e) {
			logger.error("Challenge initialization failed: " + e.getMessage());
			JsonObject root = new JsonObject();
			root.addProperty("msg", ExceptionHelper.getExceptionMessage(e));
			String json = new Gson().toJson(root);
			return Response.status(Status.UNAUTHORIZED).entity(json).build();
		}
	}

	@PUT
	@Produces(MediaType.APPLICATION_JSON)
	@Path("challenge")
	public Response validateChallenge(@Context HttpServletRequest request, String data) {

		try {

			JsonObject jsonObject = JsonParser.parseString(data).getAsJsonObject();
			String username = jsonObject.get("username").getAsString();
			String challenge = jsonObject.get("challenge").getAsString();

			RestfulStrolchComponent restComponent = RestfulStrolchComponent.getInstance();
			StrolchSessionHandler sessionHandler = restComponent.getSessionHandler();
			String source = getRemoteIp(request);
			Certificate certificate = sessionHandler.validateChallenge(username, challenge, source);
			int sessionMaxKeepAliveMinutes = sessionHandler.getSessionMaxKeepAliveMinutes();
			int cookieMaxAge = getCookieMaxAge(certificate, restComponent, sessionMaxKeepAliveMinutes);
			LocalDateTime expirationDate = LocalDateTime.now().plusSeconds(cookieMaxAge);

			JsonObject result = new JsonObject();
			String authToken = certificate.getAuthToken();
			result.addProperty("authToken", authToken);

			return setCookiesAndReturnResponse(request, restComponent, cookieMaxAge, expirationDate, result, authToken);

		} catch (PrivilegeException e) {
			logger.error("Challenge validation failed: " + e.getMessage());
			JsonObject root = new JsonObject();
			root.addProperty("msg", ExceptionHelper.getExceptionMessage(e));
			String json = new Gson().toJson(root);
			return Response.status(Status.UNAUTHORIZED).entity(json).build();
		}
	}

	private Response getAuthenticationResponse(HttpServletRequest request, Certificate certificate, String source,
			boolean setCookies) {

		RestfulStrolchComponent restComponent = RestfulStrolchComponent.getInstance();
		StrolchSessionHandler sessionHandler = restComponent.getSessionHandler();
		int sessionMaxKeepAliveMinutes = sessionHandler.getSessionMaxKeepAliveMinutes();
		int cookieMaxAge = getCookieMaxAge(certificate, restComponent, sessionMaxKeepAliveMinutes);
		LocalDateTime expirationDate = LocalDateTime.now().plusSeconds(cookieMaxAge);

		JsonObject loginResult = new JsonObject();

		PrivilegeHandler privilegeHandler = restComponent.getContainer().getPrivilegeHandler();
		PrivilegeContext privilegeContext = privilegeHandler.validate(certificate, source);
		loginResult.addProperty("sessionId", certificate.getSessionId());
		String authToken = certificate.getAuthToken();
		loginResult.addProperty("authToken", authToken);
		loginResult.addProperty("username", certificate.getUsername());
		loginResult.addProperty("firstname", certificate.getFirstname());
		loginResult.addProperty("lastname", certificate.getLastname());
		loginResult.addProperty("locale", certificate.getLocale().toLanguageTag());
		loginResult.addProperty("keepAlive", certificate.isKeepAlive());
		loginResult.addProperty("keepAliveMinutes", sessionMaxKeepAliveMinutes);
		loginResult.addProperty("cookieMaxAge", cookieMaxAge);
		loginResult.addProperty("authorizationExpiration", ISO8601.toString(expirationDate));
		loginResult.addProperty("refreshAllowed", sessionHandler.isRefreshAllowed());
		loginResult.addProperty("usage", certificate.getUsage().getValue());

		if (!certificate.getPropertyMap().isEmpty()) {
			JsonObject propObj = new JsonObject();
			loginResult.add("properties", propObj);
			for (String propKey : certificate.getPropertyMap().keySet()) {
				propObj.addProperty(propKey, certificate.getPropertyMap().get(propKey));
			}
		}

		if (!certificate.getUserRoles().isEmpty()) {
			JsonArray rolesArr = new JsonArray();
			loginResult.add("roles", rolesArr);
			for (String role : certificate.getUserRoles()) {
				rolesArr.add(new JsonPrimitive(role));
			}
		}

		if (!privilegeContext.getPrivilegeNames().isEmpty()) {
			JsonArray privArr = new JsonArray();
			loginResult.add("privileges", privArr);

			for (String name : privilegeContext.getPrivilegeNames()) {
				IPrivilege privilege = privilegeContext.getPrivilege(name);

				JsonObject privObj = new JsonObject();
				privArr.add(privObj);

				privObj.addProperty("name", name);
				privObj.addProperty("allAllowed", privilege.isAllAllowed());

				Set<String> allowSet = privilege.getAllowList();
				if (!allowSet.isEmpty()) {
					JsonArray allowArr = new JsonArray();
					privObj.add("allowList", allowArr);
					for (String allow : allowSet) {
						allowArr.add(new JsonPrimitive(allow));
					}
				}
			}
		}

		if (setCookies)
			return setCookiesAndReturnResponse(request, restComponent, cookieMaxAge, expirationDate, loginResult,
					authToken);
		return Response.ok().entity(loginResult.toString()).header(HttpHeaders.AUTHORIZATION, authToken).build();
	}

	private static Response setCookiesAndReturnResponse(HttpServletRequest request,
			RestfulStrolchComponent restComponent, int cookieMaxAge, LocalDateTime expirationDate,
			JsonObject loginResult, String authToken) {
		boolean secureCookie = restComponent.isSecureCookie();
		if (secureCookie && !request.getScheme().equals("https")) {
			String msg = "Authorization cookie is secure, but connection is not secure! Cookie won't be passed to client!";
			logger.error(msg);
		}

		String expirationDateS = ISO8601.toString(expirationDate);
		String domain = restComponent.isDomainSet() ? restComponent.getDomain() : request.getServerName();
		String path = (restComponent.isPathSet() ? restComponent.getPath() : "/") + ";SameSite=Strict";

		Date expiry = Date.from(expirationDate.atZone(ZoneId.systemDefault()).toInstant());
		boolean httpOnly = false;
		int version = 1;

		NewCookie authCookie = getNewCookie(STROLCH_AUTHORIZATION, authToken, path, domain, version,
				"Strolch Authorization header", cookieMaxAge, expiry, secureCookie, httpOnly);
		NewCookie authExpirationCookie = getNewCookie(STROLCH_AUTHORIZATION_EXPIRATION_DATE, expirationDateS, path,
				domain, version, "Strolch Authorization Expiration Date", cookieMaxAge, expiry, secureCookie, httpOnly);

		return Response.ok().entity(loginResult.toString()) //
				.header(HttpHeaders.AUTHORIZATION, authToken) //
				.cookie(authCookie) //
				.cookie(authExpirationCookie) //
				.build();
	}

	private static int getCookieMaxAge(Certificate certificate, RestfulStrolchComponent restComponent,
			int sessionMaxKeepAliveMinutes) {
		int cookieMaxAge;
		if (certificate.isKeepAlive()) {
			cookieMaxAge = (int) TimeUnit.MINUTES.toSeconds(sessionMaxKeepAliveMinutes);
		} else {
			cookieMaxAge = restComponent.getCookieMaxAge();
		}
		return cookieMaxAge;
	}

	private static NewCookie getNewCookie(String strolchAuthorization, String authToken, String path, String domain,
			int version, String comment, int cookieMaxAge, Date expiry, boolean secureCookie, boolean httpOnly) {
		return new NewCookie.Builder(strolchAuthorization) //
				.value(authToken)
				.path(path)
				.domain(domain)
				.version(version)
				.comment(comment)
				.maxAge(cookieMaxAge)
				.expiry(expiry)
				.secure(secureCookie)
				.httpOnly(httpOnly)
				.build();
	}
}
