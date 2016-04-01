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

import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.concurrent.TimeUnit;

import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.Consumes;
import javax.ws.rs.DELETE;
import javax.ws.rs.HEAD;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.HttpHeaders;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.NewCookie;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.gson.Gson;
import com.google.gson.JsonObject;

import ch.eitchnet.privilege.base.AccessDeniedException;
import ch.eitchnet.privilege.base.InvalidCredentialsException;
import ch.eitchnet.privilege.base.PrivilegeException;
import ch.eitchnet.privilege.model.Certificate;
import ch.eitchnet.privilege.model.IPrivilege;
import ch.eitchnet.privilege.model.PrivilegeContext;
import ch.eitchnet.utils.helper.StringHelper;
import li.strolch.exception.StrolchException;
import li.strolch.rest.RestfulStrolchComponent;
import li.strolch.rest.StrolchRestfulConstants;
import li.strolch.rest.StrolchSessionHandler;
import li.strolch.rest.model.Login;
import li.strolch.rest.model.LoginResult;
import li.strolch.rest.model.LogoutResult;
import li.strolch.runtime.privilege.PrivilegeHandler;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@Path("strolch/authentication")
public class AuthenticationService {

	private static final Logger logger = LoggerFactory.getLogger(AuthenticationService.class);

	@POST
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)
	public Response login(Login login, @Context HttpServletRequest request, @Context HttpHeaders headers) {

		LoginResult loginResult = new LoginResult();

		try {

			StringBuilder sb = new StringBuilder();
			if (StringHelper.isEmpty(login.getUsername()) || login.getUsername().length() < 2) {
				sb.append("Username was not given or is too short!"); //$NON-NLS-1$
			}
			if (login.getPassword() == null || login.getPassword().length < 3) {
				if (sb.length() > 0)
					sb.append("\n");
				sb.append("Password was not given or was too short!"); //$NON-NLS-1$
			}

			if (sb.length() != 0) {
				loginResult.setMsg(MessageFormat.format("Could not log in due to: {0}", sb.toString())); //$NON-NLS-1$
				return Response.status(Status.BAD_REQUEST).entity(loginResult).build();
			}

			RestfulStrolchComponent restfulStrolchComponent = RestfulStrolchComponent.getInstance();
			StrolchSessionHandler sessionHandler = restfulStrolchComponent.getComponent(StrolchSessionHandler.class);
			Certificate certificate = sessionHandler.authenticate(login.getUsername(), login.getPassword());

			PrivilegeHandler privilegeHandler = restfulStrolchComponent.getContainer().getPrivilegeHandler();
			PrivilegeContext privilegeContext = privilegeHandler.getPrivilegeContext(certificate);
			loginResult.setSessionId(certificate.getSessionId());
			loginResult.setAuthToken(certificate.getAuthToken());
			loginResult.setUsername(certificate.getUsername());
			loginResult.setFirstname(certificate.getFirstname());
			loginResult.setLastname(certificate.getLastname());
			loginResult.setLocale(certificate.getLocale());
			loginResult.setParameters(certificate.getPropertyMap());
			loginResult.setRoles(new ArrayList<>(certificate.getUserRoles()));

			List<LoginResult.Privilege> privileges = new ArrayList<>();
			for (String name : privilegeContext.getPrivilegeNames()) {
				IPrivilege privilege = privilegeContext.getPrivilege(name);
				Set<String> allowSet = privilege.getAllowList();
				ArrayList<String> allowList = null;
				if (!allowSet.isEmpty())
					allowList = new ArrayList<>(allowSet);
				privileges.add(new LoginResult.Privilege(name, privilege.isAllAllowed(), allowList));
			}
			loginResult.setPrivileges(privileges);

			boolean secureCookie = restfulStrolchComponent.isSecureCookie();
			if (secureCookie && !request.getScheme().equals("https")) {
				logger.warn(
						"Authorization cookie is secure, but connection is not secure! Cookie won't be passed to client!");
			}
			NewCookie cookie = new NewCookie(StrolchRestfulConstants.STROLCH_AUTHORIZATION, certificate.getAuthToken(),
					"/", null, "Authorization header", (int) TimeUnit.DAYS.toSeconds(1), secureCookie);

			return Response.ok().entity(loginResult)//
					.header(HttpHeaders.AUTHORIZATION, certificate.getAuthToken()).cookie(cookie).build();

		} catch (InvalidCredentialsException e) {
			logger.error(e.getMessage(), e);
			loginResult.setMsg("Could not log in as the given credentials are invalid"); //$NON-NLS-1$
			return Response.status(Status.UNAUTHORIZED).entity(loginResult).build();
		} catch (AccessDeniedException e) {
			logger.error(e.getMessage(), e);
			loginResult.setMsg(MessageFormat.format("Could not log in due to: {0}", e.getMessage())); //$NON-NLS-1$
			return Response.status(Status.UNAUTHORIZED).entity(loginResult).build();
		} catch (StrolchException | PrivilegeException e) {
			logger.error(e.getMessage(), e);
			loginResult.setMsg(MessageFormat.format("Could not log in due to: {0}", e.getMessage())); //$NON-NLS-1$
			return Response.status(Status.FORBIDDEN).entity(loginResult).build();
		} catch (Exception e) {
			logger.error(e.getMessage(), e);
			String msg = e.getMessage();
			loginResult.setMsg(MessageFormat.format("{0}: {1}", e.getClass().getName(), msg)); //$NON-NLS-1$
			return Response.serverError().entity(loginResult).build();
		}
	}

	@DELETE
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{authToken}")
	public Response logout(@PathParam("authToken") String authToken) {

		LogoutResult logoutResult = new LogoutResult();

		try {

			StrolchSessionHandler sessionHandlerHandler = RestfulStrolchComponent.getInstance()
					.getComponent(StrolchSessionHandler.class);
			Certificate certificate = sessionHandlerHandler.validate(authToken);
			sessionHandlerHandler.invalidate(certificate);

			logoutResult.setUsername(certificate.getUsername());
			logoutResult.setAuthToken(authToken);
			logoutResult.setMsg(MessageFormat.format("{0} has been logged out.", certificate.getUsername())); //$NON-NLS-1$
			return Response.ok().entity(logoutResult).build();

		} catch (StrolchException | PrivilegeException e) {
			logger.error(e.getMessage(), e);
			logoutResult.setMsg(MessageFormat.format("Could not logout due to: {0}", e.getMessage())); //$NON-NLS-1$
			return Response.status(Status.UNAUTHORIZED).entity(logoutResult).build();
		} catch (Exception e) {
			logger.error(e.getMessage(), e);
			String msg = e.getMessage();
			logoutResult.setMsg(MessageFormat.format("{0}: {1}", e.getClass().getName(), msg)); //$NON-NLS-1$
			return Response.serverError().entity(logoutResult).build();
		}
	}

	@HEAD
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{authToken}")
	public Response validateSessions(@PathParam("authToken") String authToken) {

		try {

			StrolchSessionHandler sessionHandlerHandler = RestfulStrolchComponent.getInstance()
					.getComponent(StrolchSessionHandler.class);
			sessionHandlerHandler.validate(authToken);

			return Response.ok().build();

		} catch (StrolchException | PrivilegeException e) {
			logger.error(e.getMessage(), e);
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
}
