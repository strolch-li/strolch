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
package li.strolch.rest.endpoint;

import com.google.gson.JsonArray;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.ws.rs.*;
import jakarta.ws.rs.core.Context;
import jakarta.ws.rs.core.MediaType;
import jakarta.ws.rs.core.Response;
import li.strolch.agent.api.ComponentContainer;
import li.strolch.model.json.PrivilegeElementFromJsonVisitor;
import li.strolch.model.json.PrivilegeElementToJsonVisitor;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.handler.PrivilegeHandler;
import li.strolch.privilege.model.Certificate;
import li.strolch.privilege.model.RoleRep;
import li.strolch.rest.RestfulStrolchComponent;
import li.strolch.rest.StrolchRestfulConstants;
import li.strolch.rest.helper.ResponseUtil;
import li.strolch.rest.model.ServiceResultResponse;
import li.strolch.search.StrolchValueSearch;
import li.strolch.search.ValueSearch;
import li.strolch.service.api.ServiceHandler;
import li.strolch.service.privilege.roles.*;
import li.strolch.utils.helper.StringHelper;

import static java.util.Comparator.comparing;
import static java.util.stream.Stream.concat;
import static java.util.stream.Stream.of;
import static li.strolch.privilege.handler.PrivilegeHandler.PRIVILEGE_GET_ROLE;
import static li.strolch.search.ValueSearchExpressionBuilder.containsIgnoreCase;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@Path("strolch/privilege/roles")
@Tag(name = "Privilege Roles", description = "Endpoints for managing privilege roles.")
public class PrivilegeRolesResource {

	private PrivilegeHandler getPrivilegeHandler() {
		ComponentContainer container = RestfulStrolchComponent.getInstance().getContainer();
		return container.getPrivilegeHandler().getPrivilegeHandler();
	}

	private static String getContext() {
		StackTraceElement element = new Throwable().getStackTrace()[1];
		return element.getClassName() + "." + element.getMethodName();
	}

	@Operation(summary = "Get all roles",
			description = "Retrieves a list of all privilege roles, optionally filtered by a query.", responses = {
			@ApiResponse(responseCode = "200", description = "Roles retrieved successfully.",
					content = @Content(mediaType = "application/json", schema = @Schema(type = "array"))),
			@ApiResponse(responseCode = "500", description = "Internal server error.")})
	@GET
	@Produces(MediaType.APPLICATION_JSON)
	public Response getRoles(@Context HttpServletRequest request, @QueryParam("query") String query) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);
		PrivilegeHandler privilegeHandler = getPrivilegeHandler();

		try (StrolchTransaction tx = RestfulStrolchComponent.getInstance().openTx(cert, getContext())) {
			tx.getPrivilegeContext().assertHasPrivilege(PRIVILEGE_GET_ROLE);

			PrivilegeElementToJsonVisitor visitor = new PrivilegeElementToJsonVisitor();
			ValueSearch<RoleRep> search = new StrolchValueSearch<>();
			if (StringHelper.isNotEmpty(query)) {
				String[] parts = query.split(" ");
				search = search.where(containsIgnoreCase(RoleRep::getName, parts).or(containsIgnoreCase(r -> r
						.getPrivileges()
						.values()
						.stream()
						.flatMap(f -> concat(concat(of(f.getName()), f.getAllowList().stream()),
								f.getDenyList().stream()))
						.toList(), parts)));
			}

			JsonArray rolesJ = search
					.search(privilegeHandler.getRoles(cert))
					.asStream()
					.sorted(comparing(roleRep -> roleRep.getName().toLowerCase()))
					.collect(JsonArray::new, (array, role) -> array.add(role.accept(visitor)), JsonArray::addAll);

			return Response.ok(rolesJ.toString(), MediaType.APPLICATION_JSON).build();
		}
	}

	@Operation(summary = "Get a specific role", description = "Retrieves details of a specific privilege role.",
			responses = {@ApiResponse(responseCode = "200", description = "Role details retrieved successfully.",
					content = @Content(mediaType = "application/json", schema = @Schema(type = "object"))),
					@ApiResponse(responseCode = "404", description = "Role not found."),
					@ApiResponse(responseCode = "500", description = "Internal server error.")})
	@GET
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{roleName}")
	public Response getRole(@PathParam("roleName") String roleName, @Context HttpServletRequest request) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);
		PrivilegeHandler privilegeHandler = getPrivilegeHandler();

		try (StrolchTransaction tx = RestfulStrolchComponent.getInstance().openTx(cert, getContext())) {
			tx.getPrivilegeContext().assertHasPrivilege(PRIVILEGE_GET_ROLE);

			RoleRep role = privilegeHandler.getRole(cert, roleName);
			return Response
					.ok(role.accept(new PrivilegeElementToJsonVisitor()).toString(), MediaType.APPLICATION_JSON)
					.build();
		}
	}

	@Operation(summary = "Add a new role", description = "Creates a new privilege role.", responses = {
			@ApiResponse(responseCode = "200", description = "Role created successfully.",
					content = @Content(mediaType = "application/json",
							schema = @Schema(implementation = ServiceResultResponse.class))),
			@ApiResponse(responseCode = "400", description = "Invalid request format."),
			@ApiResponse(responseCode = "500", description = "Internal server error.")})
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

	@Operation(summary = "Update a role", description = "Updates an existing privilege role.", responses = {
			@ApiResponse(responseCode = "200", description = "Role updated successfully.",
					content = @Content(mediaType = "application/json",
							schema = @Schema(implementation = ServiceResultResponse.class))),
			@ApiResponse(responseCode = "404", description = "Role not found."),
			@ApiResponse(responseCode = "500", description = "Internal server error.")})
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

	@Operation(summary = "Remove a role", description = "Deletes a privilege role from the system.", responses = {
			@ApiResponse(responseCode = "200", description = "Role removed successfully.",
					content = @Content(mediaType = "application/json",
							schema = @Schema(implementation = ServiceResultResponse.class))),
			@ApiResponse(responseCode = "404", description = "Role not found."),
			@ApiResponse(responseCode = "500", description = "Internal server error.")})
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
