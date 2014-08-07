/*
 * Copyright 2013 Robert von Burg <eitch@eitchnet.ch>
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

import java.util.Locale;

import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.Consumes;
import javax.ws.rs.DELETE;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.GenericEntity;
import javax.ws.rs.core.HttpHeaders;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;

import li.strolch.exception.StrolchException;
import li.strolch.rest.RestfulStrolchComponent;
import li.strolch.rest.StrolchSessionHandler;
import li.strolch.rest.helper.RestfulHelper;
import li.strolch.rest.model.Login;
import li.strolch.rest.model.LoginResult;
import li.strolch.rest.model.LogoutResult;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ch.eitchnet.privilege.base.PrivilegeException;
import ch.eitchnet.privilege.model.Certificate;
import ch.eitchnet.utils.helper.StringHelper;

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
		GenericEntity<LoginResult> entity = new GenericEntity<LoginResult>(loginResult, LoginResult.class) {
		};

		try {

			StringBuilder sb = new StringBuilder();
			if (StringHelper.isEmpty(login.getUsername())) {
				sb.append("Username was not given. ");
			}
			if (StringHelper.isEmpty(login.getPassword())) {
				sb.append("Password was not given.");
			}

			if (sb.length() != 0) {
				loginResult.setMsg("Could not log in due to: " + sb.toString());
				return Response.status(Status.UNAUTHORIZED).entity(loginResult).build();
			}

			StrolchSessionHandler sessionHandler = RestfulStrolchComponent.getInstance().getComponent(
					StrolchSessionHandler.class);
			String origin = request == null ? "test" : request.getRemoteAddr();
			Certificate certificate = sessionHandler.authenticate(origin, login.getUsername(), login.getPassword()
					.getBytes());

			Locale locale = RestfulHelper.getLocale(headers);
			certificate.setLocale(locale);

			loginResult.setSessionId(certificate.getAuthToken());
			loginResult.setUsername(certificate.getUsername());
			loginResult.setLocale(locale.toString());
			loginResult.setParameters(certificate.getPropertyMap());

			return Response.ok().entity(entity).build();

		} catch (StrolchException e) {
			logger.error(e.getMessage(), e);
			loginResult.setMsg("Could not log in due to: " + e.getMessage());
			return Response.status(Status.UNAUTHORIZED).entity(entity).build();
		} catch (Exception e) {
			logger.error(e.getMessage(), e);
			String msg = e.getMessage();
			loginResult.setMsg(e.getClass().getName() + ": " + msg);
			return Response.serverError().entity(entity).build();
		}
	}

	@DELETE
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{authToken}")
	public Response logout(@PathParam("authToken") String authToken, @Context HttpServletRequest request) {

		LogoutResult logoutResult = new LogoutResult();

		GenericEntity<LogoutResult> entity = new GenericEntity<LogoutResult>(logoutResult, LogoutResult.class) {
		};
		try {

			StrolchSessionHandler sessionHandlerHandler = RestfulStrolchComponent.getInstance().getComponent(
					StrolchSessionHandler.class);
			String origin = request == null ? "test" : request.getRemoteAddr();
			Certificate certificate = sessionHandlerHandler.validate(origin, authToken);
			sessionHandlerHandler.invalidateSession(origin, certificate);

			return Response.ok().entity(entity).build();

		} catch (StrolchException | PrivilegeException e) {
			logger.error(e.getMessage(), e);
			logoutResult.setMsg("Could not logout due to: " + e.getMessage());
			return Response.status(Status.UNAUTHORIZED).entity(entity).build();
		} catch (Exception e) {
			logger.error(e.getMessage(), e);
			String msg = e.getMessage();
			logoutResult.setMsg(e.getClass().getName() + ": " + msg);
			return Response.serverError().entity(entity).build();
		}
	}
}
