/*
 * Copyright (c) 2015-2024 Robert von Burg <eitch@eitchnet.ch>
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

import com.google.gson.*;
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
import li.strolch.model.Tags;
import li.strolch.model.json.PrivilegeElementFromJsonVisitor;
import li.strolch.model.json.PrivilegeElementToJsonVisitor;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.base.PasswordStrengthException;
import li.strolch.privilege.handler.PrivilegeHandler;
import li.strolch.privilege.model.Certificate;
import li.strolch.privilege.model.UserPrivileges;
import li.strolch.privilege.model.UserRep;
import li.strolch.privilege.model.UserState;
import li.strolch.rest.RestfulStrolchComponent;
import li.strolch.rest.StrolchRestfulConstants;
import li.strolch.rest.model.QueryData;
import li.strolch.rest.model.ServiceResultResponse;
import li.strolch.runtime.sessions.StrolchSessionHandler;
import li.strolch.search.SearchResult;
import li.strolch.search.ValueSearch;
import li.strolch.service.StringMapArgument;
import li.strolch.service.api.ServiceHandler;
import li.strolch.service.api.ServiceResult;
import li.strolch.service.privilege.users.*;

import java.text.MessageFormat;
import java.util.Base64;
import java.util.List;
import java.util.Locale;

import static jakarta.ws.rs.core.Response.Status.NOT_ACCEPTABLE;
import static java.util.Arrays.asList;
import static java.util.Comparator.comparing;
import static li.strolch.privilege.handler.PrivilegeHandler.PRIVILEGE_GET_USER;
import static li.strolch.privilege.handler.PrivilegeHandler.PRIVILEGE_GET_USER_PRIVILEGES;
import static li.strolch.rest.helper.ResponseUtil.toResponse;
import static li.strolch.rest.helper.RestfulHelper.toJson;
import static li.strolch.search.SearchBuilder.buildSimpleValueSearch;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@Path("strolch/privilege/users")
@Tag(name = "Privilege Users", description = "Endpoints for managing privilege users.")
public class PrivilegeUsersResource {

	private PrivilegeHandler getPrivilegeHandler() {
		ComponentContainer container = RestfulStrolchComponent.getInstance().getContainer();
		return container.getPrivilegeHandler().getPrivilegeHandler();
	}

	private static String getContext() {
		StackTraceElement element = new Throwable().getStackTrace()[1];
		return element.getClassName() + "." + element.getMethodName();
	}

	@Operation(summary = "Get all users",
			description = "Retrieves a list of all privilege users, optionally filtered by a query.", responses = {
			@ApiResponse(responseCode = "200", description = "Users retrieved successfully.",
					content = @Content(mediaType = "application/json", schema = @Schema(type = "array"))),
			@ApiResponse(responseCode = "500", description = "Internal server error.")})
	@GET
	@Produces(MediaType.APPLICATION_JSON)
	public Response queryUsers(@Context HttpServletRequest request, @BeanParam QueryData queryData) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);
		PrivilegeHandler privilegeHandler = getPrivilegeHandler();

		try (StrolchTransaction tx = RestfulStrolchComponent.getInstance().openTx(cert, getContext())) {
			tx.getPrivilegeContext().assertHasPrivilege(PRIVILEGE_GET_USER);

			String query = queryData.getQuery();
			List<UserRep> users = privilegeHandler.getUsers(cert);
			SearchResult<UserRep> result = buildSimpleValueSearch(new ValueSearch<UserRep>(), query,
					asList(UserRep::getUsername, UserRep::getFirstname, UserRep::getLastname,
							userRep -> userRep.getUserState().name(), UserRep::getRoles))
					.search(users)
					.orderBy(comparing(r -> r.getUsername().toLowerCase()));

			PrivilegeElementToJsonVisitor visitor = new PrivilegeElementToJsonVisitor();
			JsonObject root = toJson(queryData, users.size(), result, t -> t.accept(visitor));
			Gson gson = new GsonBuilder().setPrettyPrinting().create();
			return Response.ok(gson.toJson(root), MediaType.APPLICATION_JSON).build();
		}
	}

	@Operation(summary = "Query users by user representation",
			description = "Queries users based on the provided user representation in JSON format.", responses = {
			@ApiResponse(responseCode = "200", description = "Successfully retrieved users",
					content = @Content(mediaType = "application/json")),
			@ApiResponse(responseCode = "400", description = "Invalid input data"),
			@ApiResponse(responseCode = "403", description = "Forbidden - insufficient privileges"),
			@ApiResponse(responseCode = "500", description = "Internal server error")})
	@POST
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)
	@Path("query")
	public Response queryUsersByUserRep(String query, @Context HttpServletRequest request) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);
		PrivilegeHandler privilegeHandler = getPrivilegeHandler();

		try (StrolchTransaction tx = RestfulStrolchComponent.getInstance().openTx(cert, getContext())) {
			tx.getPrivilegeContext().assertHasPrivilege(PRIVILEGE_GET_USER);

			PrivilegeElementToJsonVisitor visitor = new PrivilegeElementToJsonVisitor();

			UserRep queryRep = new PrivilegeElementFromJsonVisitor().userRepFromJson(query);
			JsonArray usersArr = privilegeHandler
					.queryUsers(cert, queryRep)
					.stream()
					.sorted(comparing(r -> r.getUsername().toLowerCase()))
					.collect(JsonArray::new, (array, user) -> array.add(user.accept(visitor)), JsonArray::addAll);

			return Response.ok(usersArr.toString(), MediaType.APPLICATION_JSON).build();
		}
	}

	@Operation(summary = "Get a specific user", description = "Retrieves details of a specific privilege user.",
			responses = {@ApiResponse(responseCode = "200", description = "User details retrieved successfully.",
					content = @Content(mediaType = "application/json", schema = @Schema(type = "object"))),
					@ApiResponse(responseCode = "404", description = "User not found."),
					@ApiResponse(responseCode = "500", description = "Internal server error.")})
	@GET
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{username}")
	public Response getUser(@PathParam("username") String username, @Context HttpServletRequest request) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);
		PrivilegeHandler privilegeHandler = getPrivilegeHandler();

		try (StrolchTransaction tx = RestfulStrolchComponent.getInstance().openTx(cert, getContext())) {
			tx.getPrivilegeContext().assertHasPrivilege(PRIVILEGE_GET_USER);

			UserRep user = privilegeHandler.getUser(cert, username);
			PrivilegeElementToJsonVisitor visitor = new PrivilegeElementToJsonVisitor();
			return Response.ok(user.accept(visitor).toString(), MediaType.APPLICATION_JSON).build();
		}
	}

	@Operation(summary = "Get user privileges",
			description = "Retrieves the privileges associated with a specific user.", responses = {
			@ApiResponse(responseCode = "200", description = "User privileges retrieved successfully.",
					content = @Content(mediaType = "application/json", schema = @Schema(type = "object"))),
			@ApiResponse(responseCode = "404", description = "User not found."),
			@ApiResponse(responseCode = "500", description = "Internal server error.")})
	@GET
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{username}/privileges")
	public Response getUserPrivileges(@PathParam("username") String username, @Context HttpServletRequest request) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);
		PrivilegeHandler privilegeHandler = getPrivilegeHandler();

		try (StrolchTransaction tx = RestfulStrolchComponent.getInstance().openTx(cert, getContext())) {
			tx.getPrivilegeContext().assertHasPrivilege(PRIVILEGE_GET_USER_PRIVILEGES);

			UserPrivileges userPrivileges = privilegeHandler.getUserPrivileges(cert, username);
			PrivilegeElementToJsonVisitor visitor = new PrivilegeElementToJsonVisitor();
			return Response.ok(userPrivileges.accept(visitor).toString(), MediaType.APPLICATION_JSON).build();
		}
	}

	@Operation(summary = "Add a new user", description = "Creates a new privilege user.", responses = {
			@ApiResponse(responseCode = "200", description = "User created successfully.",
					content = @Content(mediaType = "application/json",
							schema = @Schema(implementation = ServiceResultResponse.class))),
			@ApiResponse(responseCode = "400", description = "Invalid request format."),
			@ApiResponse(responseCode = "500", description = "Internal server error.")})
	@POST
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)
	public Response addUser(String newUser, @Context HttpServletRequest request) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		ServiceHandler svcHandler = RestfulStrolchComponent.getInstance().getComponent(ServiceHandler.class);
		PrivilegeAddUserService svc = new PrivilegeAddUserService();
		PrivilegeUserArgument arg = new PrivilegeUserArgument();
		arg.user = new PrivilegeElementFromJsonVisitor().userRepFromJson(newUser);

		PrivilegeUserResult svcResult = svcHandler.doService(cert, svc, arg);
		return handleServiceResult(svcResult);
	}

	@Operation(summary = "Remove a user", description = "Deletes a privilege user from the system.", responses = {
			@ApiResponse(responseCode = "200", description = "User removed successfully.",
					content = @Content(mediaType = "application/json",
							schema = @Schema(implementation = ServiceResultResponse.class))),
			@ApiResponse(responseCode = "404", description = "User not found."),
			@ApiResponse(responseCode = "500", description = "Internal server error.")})
	@DELETE
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{username}")
	public Response removeUser(@PathParam("username") String username, @Context HttpServletRequest request) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		ServiceHandler svcHandler = RestfulStrolchComponent.getInstance().getComponent(ServiceHandler.class);
		PrivilegeRemoveUserService svc = new PrivilegeRemoveUserService();
		PrivilegeUserNameArgument arg = new PrivilegeUserNameArgument();
		arg.username = username;

		ServiceResult svcResult = svcHandler.doService(cert, svc, arg);
		return toResponse(svcResult);
	}

	@Operation(summary = "Update a user", description = "Updates an existing privilege user.", responses = {
			@ApiResponse(responseCode = "200", description = "User updated successfully.",
					content = @Content(mediaType = "application/json",
							schema = @Schema(implementation = ServiceResultResponse.class))),
			@ApiResponse(responseCode = "404", description = "User not found."),
			@ApiResponse(responseCode = "500", description = "Internal server error.")})
	@PUT
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{username}")
	public Response updateUser(@PathParam("username") String username, String updatedFields,
			@Context HttpServletRequest request) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		ServiceHandler svcHandler = RestfulStrolchComponent.getInstance().getComponent(ServiceHandler.class);
		PrivilegeUpdateUserService svc = new PrivilegeUpdateUserService();
		PrivilegeUserArgument arg = new PrivilegeUserArgument();
		arg.user = new PrivilegeElementFromJsonVisitor().userRepFromJson(updatedFields);
		if (!username.equals(arg.user.getUsername()))
			throw new IllegalArgumentException("Username mismatch");

		PrivilegeUserResult svcResult = svcHandler.doService(cert, svc, arg);
		return handleServiceResult(svcResult);
	}

	@Operation(summary = "Set user state", description = "Updates the state of a specific user.", responses = {
			@ApiResponse(responseCode = "200", description = "User state updated successfully.",
					content = @Content(mediaType = "application/json",
							schema = @Schema(implementation = ServiceResultResponse.class))),
			@ApiResponse(responseCode = "400", description = "Invalid state provided."),
			@ApiResponse(responseCode = "404", description = "User not found."),
			@ApiResponse(responseCode = "500", description = "Internal server error.")})
	@PUT
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{username}/state/{state}")
	public Response setUserState(@PathParam("username") String username, @PathParam("state") String state,
			@Context HttpServletRequest request) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		UserState userState;
		try {
			userState = UserState.valueOf(state);
		} catch (Exception e) {
			String msg = MessageFormat.format("UserState {0} is not valid!", state);
			return toResponse(msg);
		}

		ServiceHandler svcHandler = RestfulStrolchComponent.getInstance().getComponent(ServiceHandler.class);
		PrivilegeSetUserStateService svc = new PrivilegeSetUserStateService();
		PrivilegeSetUserStateArgument arg = new PrivilegeSetUserStateArgument();
		arg.username = username;
		arg.userState = userState;

		PrivilegeUserResult svcResult = svcHandler.doService(cert, svc, arg);
		return handleServiceResult(svcResult);
	}

	@Operation(summary = "Set user locale", description = "Updates the locale of a specific user.", responses = {
			@ApiResponse(responseCode = "200", description = "User locale updated successfully.",
					content = @Content(mediaType = "application/json",
							schema = @Schema(implementation = ServiceResultResponse.class))),
			@ApiResponse(responseCode = "400", description = "Invalid locale provided."),
			@ApiResponse(responseCode = "404", description = "User not found."),
			@ApiResponse(responseCode = "500", description = "Internal server error.")})
	@PUT
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{username}/locale/{locale}")
	public Response setUserLocale(@PathParam("username") String username, @PathParam("locale") String localeS,
			@Context HttpServletRequest request) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		Locale locale;
		try {
			locale = Locale.forLanguageTag(localeS);
		} catch (Exception e) {
			String msg = MessageFormat.format("Locale {0} is not valid!", localeS);
			return toResponse(msg);
		}

		ServiceHandler svcHandler = RestfulStrolchComponent.getInstance().getComponent(ServiceHandler.class);
		PrivilegeSetUserLocaleService svc = new PrivilegeSetUserLocaleService();
		PrivilegeSetUserLocaleArgument arg = new PrivilegeSetUserLocaleArgument();
		arg.username = username;
		arg.locale = locale;

		PrivilegeUserResult svcResult = svcHandler.doService(cert, svc, arg);
		return handleServiceResult(svcResult);
	}

	@Operation(summary = "Set user password", description = "Updates the password of a specific user.", responses = {
			@ApiResponse(responseCode = "200", description = "User password updated successfully.",
					content = @Content(mediaType = "application/json",
							schema = @Schema(implementation = ServiceResultResponse.class))),
			@ApiResponse(responseCode = "406", description = "Password does not meet strength requirements."),
			@ApiResponse(responseCode = "404", description = "User not found."),
			@ApiResponse(responseCode = "500", description = "Internal server error.")})
	@PUT
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{username}/password")
	public Response setUserPassword(@PathParam("username") String username, String data,
			@Context HttpServletRequest request) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);
		JsonObject jsonObject = JsonParser.parseString(data).getAsJsonObject();

		String passwordEncoded = jsonObject.get("password").getAsString();
		byte[] decode = Base64.getDecoder().decode(passwordEncoded);
		String passwordString = new String(decode);

		ServiceHandler svcHandler = RestfulStrolchComponent.getInstance().getComponent(ServiceHandler.class);
		PrivilegeSetUserPasswordService svc = new PrivilegeSetUserPasswordService();
		PrivilegeSetUserPasswordArgument arg = new PrivilegeSetUserPasswordArgument();
		arg.username = username;
		arg.password = passwordString.toCharArray();

		ServiceResult svcResult = svcHandler.doService(cert, svc, arg);
		if (svcResult.isNok()) {
			if (svcResult.getRootCause() instanceof PasswordStrengthException)
				return toResponse(NOT_ACCEPTABLE, svcResult.getRootCause());
			return toResponse(svcResult);
		}

		// if user changes their own password, then invalidate the session
		if (cert.getUsername().equals(username)) {
			StrolchSessionHandler sessionHandler = RestfulStrolchComponent.getInstance().getSessionHandler();
			sessionHandler.invalidate(cert);
		}

		return toResponse();
	}

	@Operation(summary = "Set user password state", description = "Updates the password state of a specific user.",
			responses = {@ApiResponse(responseCode = "200", description = "User password state updated successfully.",
					content = @Content(mediaType = "application/json",
							schema = @Schema(implementation = ServiceResultResponse.class))),
					@ApiResponse(responseCode = "404", description = "User not found."),
					@ApiResponse(responseCode = "500", description = "Internal server error.")})
	@PUT
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{username}/password/state")
	public Response setUserPasswordState(@PathParam("username") String username, String data,
			@Context HttpServletRequest request) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);
		JsonObject jsonObject = JsonParser.parseString(data).getAsJsonObject();

		ServiceHandler svcHandler = RestfulStrolchComponent.getInstance().getComponent(ServiceHandler.class);
		PrivilegeSetUserPasswordStateService svc = new PrivilegeSetUserPasswordStateService();
		StringMapArgument arg = svc.getArgumentInstance();
		arg.map.put(Tags.Json.USERNAME, username);
		arg.map.put(Tags.Json.STATE, jsonObject.get(Tags.Json.STATE).getAsString());

		ServiceResult svcResult = svcHandler.doService(cert, svc, arg);
		return toResponse(svcResult);
	}

	@Operation(summary = "Clear user password", description = "Removes the password of a specific user.", responses = {
			@ApiResponse(responseCode = "200", description = "User password cleared successfully.",
					content = @Content(mediaType = "application/json",
							schema = @Schema(implementation = ServiceResultResponse.class))),
			@ApiResponse(responseCode = "404", description = "User not found."),
			@ApiResponse(responseCode = "500", description = "Internal server error.")})
	@DELETE
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{username}/password")
	public Response clearUserPassword(@PathParam("username") String username, @Context HttpServletRequest request) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);
		ServiceHandler svcHandler = RestfulStrolchComponent.getInstance().getComponent(ServiceHandler.class);
		ClearUserPasswordService svc = new ClearUserPasswordService();
		PrivilegeUserNameArgument arg = svc.getArgumentInstance();
		arg.username = username;

		ServiceResult svcResult = svcHandler.doService(cert, svc, arg);
		return toResponse(svcResult);
	}

	private Response handleServiceResult(PrivilegeUserResult svcResult) {
		if (svcResult.isNok())
			return toResponse(svcResult);
		UserRep userRep = svcResult.getUser();
		PrivilegeElementToJsonVisitor visitor = new PrivilegeElementToJsonVisitor();
		return Response.ok(userRep.accept(visitor).toString(), MediaType.APPLICATION_JSON).build();
	}
}