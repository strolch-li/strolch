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
import li.strolch.privilege.base.AccessDeniedException;
import li.strolch.privilege.base.PrivilegeException;
import li.strolch.privilege.handler.PrivilegeHandler;
import li.strolch.privilege.model.Certificate;
import li.strolch.privilege.model.PrivilegeRep;
import li.strolch.privilege.model.RoleRep;
import li.strolch.rest.RestfulStrolchComponent;
import li.strolch.rest.StrolchRestfulConstants;
import li.strolch.rest.model.Result;
import li.strolch.service.api.ServiceHandler;
import li.strolch.service.privilege.roles.PrivilegeAddOrReplacePrivilegeOnRoleArgument;
import li.strolch.service.privilege.roles.PrivilegeAddOrReplacePrivilegeOnRoleService;
import li.strolch.service.privilege.roles.PrivilegeAddRoleService;
import li.strolch.service.privilege.roles.PrivilegeRemovePrivilegeFromRoleArgument;
import li.strolch.service.privilege.roles.PrivilegeRemovePrivilegeFromRoleService;
import li.strolch.service.privilege.roles.PrivilegeRemoveRoleService;
import li.strolch.service.privilege.roles.PrivilegeRoleArgument;
import li.strolch.service.privilege.roles.PrivilegeRoleNameArgument;
import li.strolch.service.privilege.roles.PrivilegeRoleResult;
import li.strolch.service.privilege.roles.PrivilegeUpdateRoleService;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@Path("strolch/privilege/roles")
public class PrivilegeRolesService {

	private PrivilegeHandler getPrivilegeHandler() {
		ComponentContainer container = RestfulStrolchComponent.getInstance().getContainer();
		return container.getPrivilegeHandler().getPrivilegeHandler();
	}

	@GET
	@Produces(MediaType.APPLICATION_JSON)
	public Response getRoles(@Context HttpServletRequest request) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);
		PrivilegeHandler privilegeHandler = getPrivilegeHandler();

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
		PrivilegeHandler privilegeHandler = getPrivilegeHandler();

		RoleRep role = privilegeHandler.getRole(cert, rolename);
		return Response.ok(role, MediaType.APPLICATION_JSON).build();
	}

	@POST
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)
	public Response addRole(RoleRep newRole, @Context HttpServletRequest request) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		ServiceHandler svcHandler = RestfulStrolchComponent.getInstance().getComponent(ServiceHandler.class);
		PrivilegeAddRoleService svc = new PrivilegeAddRoleService();
		PrivilegeRoleArgument arg = new PrivilegeRoleArgument();
		arg.role = newRole;

		PrivilegeRoleResult svcResult = svcHandler.doService(cert, svc, arg);
		return handleServiceResult(svcResult);
	}

	@PUT
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{rolename}")
	public Response replaceRole(@PathParam("rolename") String rolename, RoleRep updatedRole,
			@Context HttpServletRequest request) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		if (!rolename.equals(updatedRole.getName()))
			return Response.serverError().entity(new Result("Path rolename and data do not have same role name!"))
					.type(MediaType.APPLICATION_JSON).build();

		ServiceHandler svcHandler = RestfulStrolchComponent.getInstance().getComponent(ServiceHandler.class);
		PrivilegeUpdateRoleService svc = new PrivilegeUpdateRoleService();
		PrivilegeRoleArgument arg = new PrivilegeRoleArgument();
		arg.role = updatedRole;

		PrivilegeRoleResult svcResult = svcHandler.doService(cert, svc, arg);
		return handleServiceResult(svcResult);
	}

	@DELETE
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{rolename}")
	public Response removeRole(@PathParam("rolename") String rolename, @Context HttpServletRequest request) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		ServiceHandler svcHandler = RestfulStrolchComponent.getInstance().getComponent(ServiceHandler.class);
		PrivilegeRemoveRoleService svc = new PrivilegeRemoveRoleService();
		PrivilegeRoleNameArgument arg = new PrivilegeRoleNameArgument();
		arg.roleName = rolename;

		PrivilegeRoleResult svcResult = svcHandler.doService(cert, svc, arg);
		return handleServiceResult(svcResult);
	}

	@PUT
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{rolename}/privileges")
	public Response addOrReplacePrivilegeOnRole(@PathParam("rolename") String rolename, PrivilegeRep privilegeRep,
			@Context HttpServletRequest request) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		ServiceHandler svcHandler = RestfulStrolchComponent.getInstance().getComponent(ServiceHandler.class);
		PrivilegeAddOrReplacePrivilegeOnRoleService svc = new PrivilegeAddOrReplacePrivilegeOnRoleService();
		PrivilegeAddOrReplacePrivilegeOnRoleArgument arg = new PrivilegeAddOrReplacePrivilegeOnRoleArgument();
		arg.roleName = rolename;
		arg.privilegeRep = privilegeRep;

		PrivilegeRoleResult svcResult = svcHandler.doService(cert, svc, arg);
		return handleServiceResult(svcResult);
	}

	@DELETE
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{rolename}/privileges/{privilege}")
	public Response removePrivilegeFromRole(@PathParam("rolename") String rolename,
			@PathParam("privilege") String privilege, @Context HttpServletRequest request) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		ServiceHandler svcHandler = RestfulStrolchComponent.getInstance().getComponent(ServiceHandler.class);
		PrivilegeRemovePrivilegeFromRoleService svc = new PrivilegeRemovePrivilegeFromRoleService();
		PrivilegeRemovePrivilegeFromRoleArgument arg = new PrivilegeRemovePrivilegeFromRoleArgument();
		arg.roleName = rolename;
		arg.privilegeName = privilege;

		PrivilegeRoleResult svcResult = svcHandler.doService(cert, svc, arg);
		return handleServiceResult(svcResult);
	}

	private Response handleServiceResult(PrivilegeRoleResult svcResult) {
		if (svcResult.isOk()) {
			return Response.ok(svcResult.getRole(), MediaType.APPLICATION_JSON).build();
		} else if (svcResult.getThrowable() != null) {
			Throwable t = svcResult.getThrowable();
			if (t instanceof AccessDeniedException) {
				return Response.status(Status.FORBIDDEN).entity(new Result(t.getMessage()))
						.type(MediaType.APPLICATION_JSON).build();
			} else if (t instanceof PrivilegeException) {
				return Response.status(Status.UNAUTHORIZED).entity(new Result(t.getMessage())).build();
			}
		}

		return Response.status(Status.INTERNAL_SERVER_ERROR).entity(new Result(svcResult.getMessage())).build();
	}
}
