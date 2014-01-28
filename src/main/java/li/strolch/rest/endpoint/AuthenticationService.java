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

import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BeanParam;
import javax.ws.rs.Consumes;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.GenericEntity;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import li.strolch.exception.StrolchException;
import li.strolch.rest.RestfulStrolchComponent;
import li.strolch.rest.StrolchSessionHandler;
import li.strolch.rest.form.LoginForm;
import li.strolch.rest.form.LogoutForm;
import li.strolch.rest.model.LoginResult;
import li.strolch.rest.model.LogoutResult;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ch.eitchnet.privilege.model.Certificate;
import ch.eitchnet.utils.helper.StringHelper;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@Path("strolch/authentication")
public class AuthenticationService {

	private static final Logger logger = LoggerFactory.getLogger(AuthenticationService.class);

	@Context
	HttpServletRequest servletRequest;

	@POST
	@Consumes(MediaType.APPLICATION_FORM_URLENCODED)
	@Produces(MediaType.APPLICATION_JSON)
	@Path("login")
	public Response login(@BeanParam LoginForm loginForm) {

		LoginResult loginResult = new LoginResult();
		GenericEntity<LoginResult> entity = new GenericEntity<LoginResult>(loginResult, LoginResult.class) {
		};
		try {

			StringBuilder sb = new StringBuilder();
			if (StringHelper.isEmpty(loginForm.getUsername())) {
				sb.append("Username was not given. ");
			}
			if (StringHelper.isEmpty(loginForm.getPassword())) {
				sb.append("Password was not given.");
			}

			if (sb.length() != 0) {
				loginResult.setMsg("Could not log in due to: " + sb.toString());
				return Response.status(401).entity(loginResult).build();
			}

			StrolchSessionHandler sessionHandler = RestfulStrolchComponent.getInstance().getComponent(
					StrolchSessionHandler.class);
			String origin = getOrigin();
			Certificate certificate = sessionHandler.authenticate(origin, loginForm.getUsername(), loginForm
					.getPassword().getBytes());

			loginResult.setSessionId(certificate.getAuthToken());
			loginResult.setUsername(certificate.getUsername());
			loginResult.setLocale(certificate.getLocale().getLanguage() + "_" + certificate.getLocale().getCountry());
			loginResult.setParameters(certificate.getPropertyMap());

			return Response.ok().entity(entity).build();
		} catch (StrolchException e) {
			loginResult.setMsg("Could not log in due to: " + e.getMessage());
			return Response.status(401).entity(entity).build();
		} catch (Exception e) {
			logger.error(e.getMessage(), e);
			String msg = e.getMessage();
			loginResult.setMsg(e.getClass().getName() + ": " + msg);
			return Response.serverError().entity(entity).build();
		}
	}

	private String getOrigin() {
		if (servletRequest == null)
			return "test";
		return servletRequest.getRequestedSessionId();
	}

	@POST
	@Consumes(MediaType.APPLICATION_FORM_URLENCODED)
	@Produces(MediaType.APPLICATION_JSON)
	@Path("logout")
	public Response logout(@BeanParam LogoutForm logoutForm) {

		LogoutResult logoutResult = new LogoutResult();

		GenericEntity<LogoutResult> entity = new GenericEntity<LogoutResult>(logoutResult, LogoutResult.class) {
		};
		try {

			StringBuilder sb = new StringBuilder();
			if (StringHelper.isEmpty(logoutForm.getUsername())) {
				sb.append("Username was not given. ");
			}
			if (StringHelper.isEmpty(logoutForm.getSessionId())) {
				sb.append("SessionId was not given. ");
			}
			if (sb.length() != 0) {
				logoutResult.setMsg("Could not logout due to: " + sb.toString());
				return Response.status(401).entity(logoutResult).build();
			}

			StrolchSessionHandler sessionHandlerHandler = RestfulStrolchComponent.getInstance().getComponent(
					StrolchSessionHandler.class);
			String origin = getOrigin();
			Certificate certificate = sessionHandlerHandler.validate(origin, logoutForm.getUsername(),
					logoutForm.getSessionId());
			sessionHandlerHandler.invalidateSession(origin, certificate);

			return Response.ok().entity(entity).build();
		} catch (Exception e) {
			logger.error(e.getMessage(), e);
			String msg = e.getMessage();
			logoutResult.setMsg(e.getClass().getName() + ": " + msg);
			return Response.serverError().entity(entity).build();
		}
	}
}
