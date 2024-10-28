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

import com.google.gson.*;
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
public class PrivilegeUsersResource {

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