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

import java.util.List;

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
import li.strolch.rest.model.Result;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ch.eitchnet.privilege.base.AccessDeniedException;
import ch.eitchnet.privilege.base.PrivilegeException;
import ch.eitchnet.privilege.handler.PrivilegeHandler;
import ch.eitchnet.privilege.model.Certificate;
import ch.eitchnet.privilege.model.PrivilegeRep;
import ch.eitchnet.privilege.model.RoleRep;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@Path("strolch/privilege/roles")
public class PrivilegeRolesService {

	private static final Logger logger = LoggerFactory.getLogger(PrivilegeRolesService.class);

	private PrivilegeHandler getPrivilegeHandler(Certificate cert) {
		ComponentContainer container = RestfulStrolchComponent.getInstance().getContainer();
		return container.getPrivilegeHandler().getPrivilegeHandler(cert);
	}

	@GET
	@Produces(MediaType.APPLICATION_JSON)
	public Response getRoles(@Context HttpServletRequest request) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);
		PrivilegeHandler privilegeHandler = getPrivilegeHandler(cert);

		List<RoleRep> roles = privilegeHandler.getRoles(cert);
		GenericEntity<List<RoleRep>> entity = new GenericEntity<List<RoleRep>>(roles) {
		};
		return Response.ok(entity, MediaType.APPLICATION_JSON).build();
	}

	@GET
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{rolename}")
	public Response getRole(@PathParam("rolename") String rolename, @Context HttpServletRequest request) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);
		PrivilegeHandler privilegeHandler = getPrivilegeHandler(cert);

		RoleRep role = privilegeHandler.getRole(cert, rolename);
		return Response.ok(role, MediaType.APPLICATION_JSON).build();
	}

	@POST
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)
	public Response addRole(RoleRep newRole, @Context HttpServletRequest request) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);
		try {

			PrivilegeHandler privilegeHandler = getPrivilegeHandler(cert);
			RoleRep role = privilegeHandler.addRole(cert, newRole);
			return Response.ok(role, MediaType.APPLICATION_JSON).build();

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
	@Path("{rolename}")
	public Response replaceRole(@PathParam("rolename") String rolename, RoleRep updatedRole,
			@Context HttpServletRequest request) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);
		try {

			if (!rolename.equals(updatedRole.getName()))
				return Response.serverError().entity(new Result("Path rolename and data do not have same role name!"))
						.type(MediaType.APPLICATION_JSON).build();

			PrivilegeHandler privilegeHandler = getPrivilegeHandler(cert);
			RoleRep role = privilegeHandler.replaceRole(cert, updatedRole);
			return Response.ok(role, MediaType.APPLICATION_JSON).build();

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
	@Path("{rolename}")
	public Response removeRole(@PathParam("rolename") String rolename, @Context HttpServletRequest request) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);
		try {

			PrivilegeHandler privilegeHandler = getPrivilegeHandler(cert);
			RoleRep role = privilegeHandler.removeRole(cert, rolename);
			return Response.ok(role, MediaType.APPLICATION_JSON).build();

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
	@Path("{rolename}/privileges")
	public Response addOrReplacePrivilegeOnRole(@PathParam("rolename") String rolename, PrivilegeRep privilegeRep,
			@Context HttpServletRequest request) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);
		try {

			PrivilegeHandler privilegeHandler = getPrivilegeHandler(cert);
			RoleRep updatedRole = privilegeHandler.addOrReplacePrivilegeOnRole(cert, rolename, privilegeRep);
			return Response.ok(updatedRole, MediaType.APPLICATION_JSON).build();

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
	@Path("{rolename}/privileges/{privilege}")
	public Response removePrivilegeFromRole(@PathParam("rolename") String rolename,
			@PathParam("privilege") String privilege, @Context HttpServletRequest request) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);
		try {

			PrivilegeHandler privilegeHandler = getPrivilegeHandler(cert);
			RoleRep updatedRole = privilegeHandler.removePrivilegeFromRole(cert, rolename, privilege);
			return Response.ok(updatedRole, MediaType.APPLICATION_JSON).build();

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
