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

import static li.strolch.rest.StrolchRestfulConstants.ROLE_STROLCH_PRIVILEGE_ADMIN;

import java.text.MessageFormat;
import java.util.List;
import java.util.Locale;

import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.Consumes;
import javax.ws.rs.DELETE;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.GenericEntity;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.rest.RestfulStrolchComponent;
import li.strolch.rest.StrolchRestfulConstants;
import li.strolch.rest.model.PasswordField;
import li.strolch.rest.model.Result;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ch.eitchnet.privilege.base.AccessDeniedException;
import ch.eitchnet.privilege.base.PrivilegeException;
import ch.eitchnet.privilege.handler.PrivilegeHandler;
import ch.eitchnet.privilege.model.Certificate;
import ch.eitchnet.privilege.model.UserRep;
import ch.eitchnet.privilege.model.UserState;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@Path("strolch/privilege/users")
public class PrivilegeUsersService {

	private static final Logger logger = LoggerFactory.getLogger(PrivilegeUsersService.class);

	private PrivilegeHandler getPrivilegeHandler(Certificate cert, boolean requiresStrolchPrivilegeAdminRole) {
		if (requiresStrolchPrivilegeAdminRole && !cert.hasRole(ROLE_STROLCH_PRIVILEGE_ADMIN)) {
			String msg = "You may not perform the request as you are missing role {0}";
			throw new AccessDeniedException(MessageFormat.format(msg, ROLE_STROLCH_PRIVILEGE_ADMIN));
		}

		ComponentContainer container = RestfulStrolchComponent.getInstance().getContainer();
		return container.getPrivilegeHandler().getPrivilegeHandler(cert);
	}

	@GET
	@Produces(MediaType.APPLICATION_JSON)
	public Response getUsers(@Context HttpServletRequest request) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);
		PrivilegeHandler privilegeHandler = getPrivilegeHandler(cert, true);

		List<UserRep> users = privilegeHandler.getUsers(cert);
		GenericEntity<List<UserRep>> entity = new GenericEntity<List<UserRep>>(users) {
		};
		return Response.ok(entity, MediaType.APPLICATION_JSON).build();
	}

	@GET
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{username}")
	public Response getUser(@PathParam("username") String username, @Context HttpServletRequest request) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);
		PrivilegeHandler privilegeHandler = getPrivilegeHandler(cert, true);

		UserRep user = privilegeHandler.getUser(cert, username);
		return Response.ok(user, MediaType.APPLICATION_JSON).build();
	}

	@POST
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)
	@Path("query")
	public Response queryUsers(UserRep query, @Context HttpServletRequest request) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);
		PrivilegeHandler privilegeHandler = getPrivilegeHandler(cert, true);

		List<UserRep> users = privilegeHandler.queryUsers(cert, query);
		GenericEntity<List<UserRep>> entity = new GenericEntity<List<UserRep>>(users) {
		};
		return Response.ok(entity, MediaType.APPLICATION_JSON).build();
	}

	@POST
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)
	public Response addUser(UserRep newUser, @Context HttpServletRequest request) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);
		try {

			PrivilegeHandler privilegeHandler = getPrivilegeHandler(cert, true);
			UserRep user = privilegeHandler.addUser(cert, newUser, null);
			return Response.ok(user, MediaType.APPLICATION_JSON).build();

		} catch (AccessDeniedException e) {
			logger.error(e.getMessage(), e);
			return Response.status(Status.UNAUTHORIZED).entity(new Result(e.getMessage()))
					.type(MediaType.APPLICATION_JSON).build();
		} catch (PrivilegeException e) {
			logger.error(e.getMessage(), e);
			return Response.status(Status.FORBIDDEN).entity(new Result(e.getMessage()))
					.type(MediaType.APPLICATION_JSON).build();
		}
	}

	@DELETE
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{username}")
	public Response removeUser(@PathParam("username") String username, @Context HttpServletRequest request) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);
		try {

			PrivilegeHandler privilegeHandler = getPrivilegeHandler(cert, true);
			UserRep user = privilegeHandler.removeUser(cert, username);
			return Response.ok(user, MediaType.APPLICATION_JSON).build();

		} catch (AccessDeniedException e) {
			logger.error(e.getMessage(), e);
			return Response.status(Status.UNAUTHORIZED).entity(new Result(e.getMessage()))
					.type(MediaType.APPLICATION_JSON).build();
		} catch (PrivilegeException e) {
			logger.error(e.getMessage(), e);
			return Response.status(Status.FORBIDDEN).entity(new Result(e.getMessage()))
					.type(MediaType.APPLICATION_JSON).build();
		}
	}

	@PUT
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{username}")
	public Response updateUser(@PathParam("username") String username, UserRep updatedFields,
			@Context HttpServletRequest request) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);
		try {

			if (!username.equals(updatedFields.getUsername()))
				return Response.serverError().entity(new Result("Path username and data do not have same username!"))
						.type(MediaType.APPLICATION_JSON).build();

			PrivilegeHandler privilegeHandler = getPrivilegeHandler(cert, true);
			UserRep updatedUser = privilegeHandler.updateUser(cert, updatedFields);
			return Response.ok(updatedUser, MediaType.APPLICATION_JSON).build();

		} catch (AccessDeniedException e) {
			logger.error(e.getMessage(), e);
			return Response.status(Status.UNAUTHORIZED).entity(new Result(e.getMessage()))
					.type(MediaType.APPLICATION_JSON).build();
		} catch (PrivilegeException e) {
			logger.error(e.getMessage(), e);
			return Response.status(Status.FORBIDDEN).entity(new Result(e.getMessage()))
					.type(MediaType.APPLICATION_JSON).build();
		}
	}

	@PUT
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{username}/roles/{rolename}")
	public Response addRoleToUser(@PathParam("username") String username, @PathParam("rolename") String rolename,
			@Context HttpServletRequest request) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);
		try {

			PrivilegeHandler privilegeHandler = getPrivilegeHandler(cert, true);
			UserRep updatedUser = privilegeHandler.addRoleToUser(cert, username, rolename);
			return Response.ok(updatedUser, MediaType.APPLICATION_JSON).build();

		} catch (AccessDeniedException e) {
			logger.error(e.getMessage(), e);
			return Response.status(Status.UNAUTHORIZED).entity(new Result(e.getMessage()))
					.type(MediaType.APPLICATION_JSON).build();
		} catch (PrivilegeException e) {
			logger.error(e.getMessage(), e);
			return Response.status(Status.FORBIDDEN).entity(new Result(e.getMessage()))
					.type(MediaType.APPLICATION_JSON).build();
		}
	}

	@DELETE
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{username}/roles/{rolename}")
	public Response removeRoleFromUser(@PathParam("username") String username, @PathParam("rolename") String rolename,
			@Context HttpServletRequest request) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);
		try {

			PrivilegeHandler privilegeHandler = getPrivilegeHandler(cert, true);
			UserRep updatedUser = privilegeHandler.removeRoleFromUser(cert, username, rolename);
			return Response.ok(updatedUser, MediaType.APPLICATION_JSON).build();

		} catch (AccessDeniedException e) {
			logger.error(e.getMessage(), e);
			return Response.status(Status.UNAUTHORIZED).entity(new Result(e.getMessage()))
					.type(MediaType.APPLICATION_JSON).build();
		} catch (PrivilegeException e) {
			logger.error(e.getMessage(), e);
			return Response.status(Status.FORBIDDEN).entity(new Result(e.getMessage()))
					.type(MediaType.APPLICATION_JSON).build();
		}
	}

	@PUT
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{username}/state/{state}")
	public Response setUserState(@PathParam("username") String username, @PathParam("state") String state,
			@Context HttpServletRequest request) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);
		try {

			UserState userState;
			try {
				userState = UserState.valueOf(state);
			} catch (Exception e) {
				String msg = MessageFormat.format("UserState {0} is not valid!", state);
				return Response.serverError().entity(new Result(msg)).type(MediaType.APPLICATION_JSON).build();
			}

			PrivilegeHandler privilegeHandler = getPrivilegeHandler(cert, true);
			UserRep updatedUser = privilegeHandler.setUserState(cert, username, userState);
			return Response.ok(updatedUser, MediaType.APPLICATION_JSON).build();

		} catch (AccessDeniedException e) {
			logger.error(e.getMessage(), e);
			return Response.status(Status.UNAUTHORIZED).entity(new Result(e.getMessage()))
					.type(MediaType.APPLICATION_JSON).build();
		} catch (PrivilegeException e) {
			logger.error(e.getMessage(), e);
			return Response.status(Status.FORBIDDEN).entity(new Result(e.getMessage()))
					.type(MediaType.APPLICATION_JSON).build();
		}
	}

	@PUT
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{username}/password")
	public Response setUserPassword(@PathParam("username") String username, PasswordField passwordField,
			@Context HttpServletRequest request) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);
		try {

			// if user changing own password, then no need for StrolchPrivilegeAdmin
			PrivilegeHandler privilegeHandler = getPrivilegeHandler(cert, !cert.getUsername().equals(username));

			privilegeHandler.setUserPassword(cert, username, passwordField.getPassword().getBytes());
			return Response.ok(new Result(), MediaType.APPLICATION_JSON).build();

		} catch (AccessDeniedException e) {
			logger.error(e.getMessage(), e);
			return Response.status(Status.UNAUTHORIZED).entity(new Result(e.getMessage()))
					.type(MediaType.APPLICATION_JSON).build();
		} catch (PrivilegeException e) {
			logger.error(e.getMessage(), e);
			return Response.status(Status.FORBIDDEN).entity(new Result(e.getMessage()))
					.type(MediaType.APPLICATION_JSON).build();
		}
	}

	@PUT
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{username}/locale/{locale}")
	public Response setUserLocale(@PathParam("username") String username, @PathParam("locale") String localeS,
			@Context HttpServletRequest request) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);
		try {

			Locale locale;
			try {
				locale = new Locale(localeS);
			} catch (Exception e) {
				String msg = MessageFormat.format("Locale {0} is not valid!", localeS);
				return Response.serverError().entity(new Result(msg)).type(MediaType.APPLICATION_JSON).build();
			}

			// if user changing own locale, then no need for StrolchPrivilegeAdmin
			PrivilegeHandler privilegeHandler = getPrivilegeHandler(cert, !cert.getUsername().equals(username));

			UserRep updatedUser = privilegeHandler.setUserLocale(cert, username, locale);
			return Response.ok(updatedUser, MediaType.APPLICATION_JSON).build();

		} catch (AccessDeniedException e) {
			logger.error(e.getMessage(), e);
			return Response.status(Status.UNAUTHORIZED).entity(new Result(e.getMessage()))
					.type(MediaType.APPLICATION_JSON).build();
		} catch (PrivilegeException e) {
			logger.error(e.getMessage(), e);
			return Response.status(Status.FORBIDDEN).entity(new Result(e.getMessage()))
					.type(MediaType.APPLICATION_JSON).build();
		}
	}
}
