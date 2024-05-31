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

import com.google.gson.JsonArray;
import com.google.gson.JsonParser;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.ws.rs.*;
import jakarta.ws.rs.core.Context;
import jakarta.ws.rs.core.MediaType;
import jakarta.ws.rs.core.Response;
import li.strolch.agent.api.ComponentContainer;
import li.strolch.model.json.PrivilegeElementToJsonVisitor;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.handler.PrivilegeHandler;
import li.strolch.privilege.model.Certificate;
import li.strolch.privilege.model.Group;
import li.strolch.privilege.model.GroupPrivileges;
import li.strolch.privilege.model.UserPrivileges;
import li.strolch.rest.RestfulStrolchComponent;
import li.strolch.rest.StrolchRestfulConstants;
import li.strolch.search.StrolchValueSearch;
import li.strolch.search.ValueSearch;
import li.strolch.service.JsonServiceArgument;
import li.strolch.service.JsonServiceResult;
import li.strolch.service.StringArgument;
import li.strolch.service.api.ServiceHandler;
import li.strolch.service.privilege.groups.PrivilegeAddGroupService;
import li.strolch.service.privilege.groups.PrivilegeRemoveGroupService;
import li.strolch.service.privilege.groups.PrivilegeUpdateGroupService;
import li.strolch.utils.helper.StringHelper;

import static java.util.Comparator.comparing;
import static li.strolch.privilege.handler.PrivilegeHandler.*;
import static li.strolch.rest.helper.ResponseUtil.toResponse;
import static li.strolch.search.ValueSearchExpressionBuilder.containsIgnoreCase;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@Path("strolch/privilege/groups")
public class PrivilegeGroupsResource {

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
	public Response getGroups(@Context HttpServletRequest request, @QueryParam("query") String query) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);
		PrivilegeHandler privilegeHandler = getPrivilegeHandler();

		try (StrolchTransaction tx = RestfulStrolchComponent.getInstance().openTx(cert, getContext())) {
			tx.getPrivilegeContext().assertHasPrivilege(PRIVILEGE_GET_GROUP);

			PrivilegeElementToJsonVisitor visitor = new PrivilegeElementToJsonVisitor();
			ValueSearch<Group> search = new StrolchValueSearch<>();
			if (StringHelper.isNotEmpty(query)) {
				String[] parts = query.split(" ");
				search = search.where(containsIgnoreCase(Group::name, parts).or(
						containsIgnoreCase(g -> g.roles().stream().toList(), parts)));
			}

			JsonArray groupsJ = search
					.search(privilegeHandler.getGroups(cert))
					.asStream()
					.sorted(comparing(g -> g.name().toLowerCase()))
					.collect(JsonArray::new, (array, g) -> array.add(g.accept(visitor)), JsonArray::addAll);

			return Response.ok(groupsJ.toString(), MediaType.APPLICATION_JSON).build();
		}
	}

	@GET
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{group}")
	public Response getGroup(@PathParam("group") String groupName, @Context HttpServletRequest request) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);
		PrivilegeHandler privilegeHandler = getPrivilegeHandler();

		try (StrolchTransaction tx = RestfulStrolchComponent.getInstance().openTx(cert, getContext())) {
			tx.getPrivilegeContext().assertHasPrivilege(PRIVILEGE_GET_USER);

			Group group = privilegeHandler.getGroup(cert, groupName);
			PrivilegeElementToJsonVisitor visitor = new PrivilegeElementToJsonVisitor();
			return Response.ok(group.accept(visitor).toString(), MediaType.APPLICATION_JSON).build();
		}
	}

	@GET
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{group}/privileges")
	public Response getGroupPrivileges(@PathParam("group") String group, @Context HttpServletRequest request) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);
		PrivilegeHandler privilegeHandler = getPrivilegeHandler();

		try (StrolchTransaction tx = RestfulStrolchComponent.getInstance().openTx(cert, getContext())) {
			tx.getPrivilegeContext().assertHasPrivilege(PRIVILEGE_GET_GROUP_PRIVILEGES);

			GroupPrivileges groupPrivileges = privilegeHandler.getGroupPrivileges(cert, group);
			PrivilegeElementToJsonVisitor visitor = new PrivilegeElementToJsonVisitor();
			return Response.ok(groupPrivileges.accept(visitor).toString(), MediaType.APPLICATION_JSON).build();
		}
	}

	@POST
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)
	public Response addGroup(@Context HttpServletRequest request, String data) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		ServiceHandler svcHandler = RestfulStrolchComponent.getInstance().getComponent(ServiceHandler.class);
		PrivilegeAddGroupService svc = new PrivilegeAddGroupService();
		JsonServiceArgument arg = svc.getArgumentInstance();
		arg.jsonElement = JsonParser.parseString(data).getAsJsonObject();

		JsonServiceResult svcResult = svcHandler.doService(cert, svc, arg);
		return toResponse(svcResult);
	}

	@PUT
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{group}")
	public Response updateGroup(@Context HttpServletRequest request, @PathParam("group") String group, String data) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		ServiceHandler svcHandler = RestfulStrolchComponent.getInstance().getComponent(ServiceHandler.class);
		PrivilegeUpdateGroupService svc = new PrivilegeUpdateGroupService();
		JsonServiceArgument arg = svc.getArgumentInstance();
		arg.objectId = group;
		arg.jsonElement = JsonParser.parseString(data).getAsJsonObject();

		JsonServiceResult svcResult = svcHandler.doService(cert, svc, arg);
		return toResponse(svcResult);
	}

	@DELETE
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{group}")
	public Response removeGroup(@Context HttpServletRequest request, @PathParam("group") String group) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		ServiceHandler svcHandler = RestfulStrolchComponent.getInstance().getComponent(ServiceHandler.class);
		PrivilegeRemoveGroupService svc = new PrivilegeRemoveGroupService();
		StringArgument arg = new StringArgument();
		arg.value = group;

		JsonServiceResult svcResult = svcHandler.doService(cert, svc, arg);
		return toResponse(svcResult);
	}
}
