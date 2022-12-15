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

import static java.util.Comparator.comparing;
import static li.strolch.privilege.handler.PrivilegeHandler.PRIVILEGE_GET_ROLE;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.ws.rs.*;
import jakarta.ws.rs.core.Context;
import jakarta.ws.rs.core.MediaType;
import jakarta.ws.rs.core.Response;

import com.google.gson.JsonArray;
import li.strolch.agent.api.ComponentContainer;
import li.strolch.model.json.PrivilegeElementFromJsonVisitor;
import li.strolch.model.json.PrivilegeElementToJsonVisitor;
import li.strolch.persistence.api.StrolchTransaction;
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

	private static String getContext() {
		StackTraceElement element = new Throwable().getStackTrace()[1];
		return element.getClassName() + "." + element.getMethodName();
	}

	@GET
	@Produces(MediaType.APPLICATION_JSON)
	public Response getRoles(@Context HttpServletRequest request) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);
		PrivilegeHandler privilegeHandler = getPrivilegeHandler();

		try (StrolchTransaction tx = RestfulStrolchComponent.getInstance().openTx(cert, getContext())) {
			tx.getPrivilegeContext().assertHasPrivilege(PRIVILEGE_GET_ROLE);

			PrivilegeElementToJsonVisitor visitor = new PrivilegeElementToJsonVisitor();
			JsonArray rolesJ = privilegeHandler.getRoles(cert).stream() //
					.sorted(comparing(roleRep -> roleRep.getName().toLowerCase())) //
					.collect(JsonArray::new, //
							(array, role) -> array.add(role.accept(visitor)), //
							JsonArray::addAll);

			return Response.ok(rolesJ.toString(), MediaType.APPLICATION_JSON).build();
		}
	}

	@GET
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{roleName}")
	public Response getRole(@PathParam("roleName") String roleName, @Context HttpServletRequest request) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);
		PrivilegeHandler privilegeHandler = getPrivilegeHandler();

		try (StrolchTransaction tx = RestfulStrolchComponent.getInstance().openTx(cert, getContext())) {
			tx.getPrivilegeContext().assertHasPrivilege(PRIVILEGE_GET_ROLE);

			RoleRep role = privilegeHandler.getRole(cert, roleName);
			return Response.ok(role.accept(new PrivilegeElementToJsonVisitor()).toString(), MediaType.APPLICATION_JSON)
					.build();
		}
	}

	@POST
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)
	public Response addRole(@Context HttpServletRequest request, String data) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		RoleRep newRole = new PrivilegeElementFromJsonVisitor().roleRepFromJson(data);

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
	@Path("{roleName}")
	public Response replaceRole(@Context HttpServletRequest request, @PathParam("roleName") String roleName,
			String data) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		RoleRep updatedRole = new PrivilegeElementFromJsonVisitor().roleRepFromJson(data);
		if (!roleName.equals(updatedRole.getName())) {
			String msg = "Path roleName and data do not have same role name!";
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
	@Path("{roleName}")
	public Response removeRole(@Context HttpServletRequest request, @PathParam("roleName") String roleName) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		ServiceHandler svcHandler = RestfulStrolchComponent.getInstance().getComponent(ServiceHandler.class);
		PrivilegeRemoveRoleService svc = new PrivilegeRemoveRoleService();
		PrivilegeRoleNameArgument arg = new PrivilegeRoleNameArgument();
		arg.roleName = roleName;

		PrivilegeRoleResult svcResult = svcHandler.doService(cert, svc, arg);
		return handleServiceResult(svcResult);
	}

	@PUT
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{roleName}/privileges")
	public Response addOrReplacePrivilegeOnRole(@Context HttpServletRequest request,
			@PathParam("roleName") String roleName, String data) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		PrivilegeRep privilegeRep = new PrivilegeElementFromJsonVisitor().privilegeRepFromJson(data);

		ServiceHandler svcHandler = RestfulStrolchComponent.getInstance().getComponent(ServiceHandler.class);
		PrivilegeAddOrReplacePrivilegeOnRoleService svc = new PrivilegeAddOrReplacePrivilegeOnRoleService();
		PrivilegeAddOrReplacePrivilegeOnRoleArgument arg = new PrivilegeAddOrReplacePrivilegeOnRoleArgument();
		arg.roleName = roleName;
		arg.privilegeRep = privilegeRep;

		PrivilegeRoleResult svcResult = svcHandler.doService(cert, svc, arg);
		return handleServiceResult(svcResult);
	}

	@DELETE
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{roleName}/privileges/{privilege}")
	public Response removePrivilegeFromRole(@Context HttpServletRequest request, @PathParam("roleName") String roleName,
			@PathParam("privilege") String privilege) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		ServiceHandler svcHandler = RestfulStrolchComponent.getInstance().getComponent(ServiceHandler.class);
		PrivilegeRemovePrivilegeFromRoleService svc = new PrivilegeRemovePrivilegeFromRoleService();
		PrivilegeRemovePrivilegeFromRoleArgument arg = new PrivilegeRemovePrivilegeFromRoleArgument();
		arg.roleName = roleName;
		arg.privilegeName = privilege;

		PrivilegeRoleResult svcResult = svcHandler.doService(cert, svc, arg);
		return handleServiceResult(svcResult);
	}

	private Response handleServiceResult(PrivilegeRoleResult svcResult) {
		if (svcResult.isOk()) {
			RoleRep roleRep = svcResult.getRole();
			return Response
					.ok(roleRep.accept(new PrivilegeElementToJsonVisitor()).toString(), MediaType.APPLICATION_JSON)
					.build();
		}
		return ResponseUtil.toResponse(svcResult);
	}
}
