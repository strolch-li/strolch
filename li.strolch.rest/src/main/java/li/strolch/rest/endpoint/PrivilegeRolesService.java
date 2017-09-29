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

import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.*;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.util.List;

import com.google.gson.JsonArray;
import li.strolch.agent.api.ComponentContainer;
import li.strolch.model.json.PrivilegeElementToJsonVisitor;
import li.strolch.privilege.handler.PrivilegeHandler;
import li.strolch.privilege.model.Certificate;
import li.strolch.privilege.model.PrivilegeRep;
import li.strolch.privilege.model.RoleRep;
import li.strolch.rest.RestfulStrolchComponent;
import li.strolch.rest.StrolchRestfulConstants;
import li.strolch.rest.helper.ResponseUtil;
import li.strolch.service.api.ServiceHandler;
import li.strolch.service.privilege.roles.*;

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
		JsonArray rolesJ = toJson(roles);
		return Response.ok(rolesJ.toString(), MediaType.APPLICATION_JSON).build();
	}

	@GET
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{rolename}")
	public Response getRole(@PathParam("rolename") String rolename, @Context HttpServletRequest request) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);
		PrivilegeHandler privilegeHandler = getPrivilegeHandler();

		RoleRep role = privilegeHandler.getRole(cert, rolename);
		return Response.ok(role.accept(new PrivilegeElementToJsonVisitor()), MediaType.APPLICATION_JSON).build();
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

		if (!rolename.equals(updatedRole.getName())) {
			String msg = "Path rolename and data do not have same role name!";
			return ResponseUtil.toResponse(msg);
		}

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
			RoleRep roleRep = svcResult.getRole();
			return Response.ok(roleRep.accept(new PrivilegeElementToJsonVisitor()), MediaType.APPLICATION_JSON).build();
		}
		return ResponseUtil.toResponse(svcResult);
	}

	private JsonArray toJson(List<RoleRep> roles) {
		JsonArray rolesArr = new JsonArray();
		for (RoleRep roleRep : roles) {
			rolesArr.add(roleRep.accept(new PrivilegeElementToJsonVisitor()));
		}
		return rolesArr;
	}
}
