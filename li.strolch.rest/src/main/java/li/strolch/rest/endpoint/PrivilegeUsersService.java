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
import static li.strolch.privilege.handler.PrivilegeHandler.PRIVILEGE_GET_USER;
import static li.strolch.rest.helper.RestfulHelper.toJson;
import static li.strolch.search.SearchBuilder.buildSimpleValueSearch;

import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.*;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.text.MessageFormat;
import java.util.Arrays;
import java.util.Base64;
import java.util.List;
import java.util.Locale;

import com.google.gson.*;
import li.strolch.agent.api.ComponentContainer;
import li.strolch.model.json.PrivilegeElementFromJsonVisitor;
import li.strolch.model.json.PrivilegeElementToJsonVisitor;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.handler.PrivilegeHandler;
import li.strolch.privilege.model.Certificate;
import li.strolch.privilege.model.UserRep;
import li.strolch.privilege.model.UserState;
import li.strolch.rest.RestfulStrolchComponent;
import li.strolch.rest.StrolchRestfulConstants;
import li.strolch.rest.StrolchSessionHandler;
import li.strolch.rest.helper.ResponseUtil;
import li.strolch.rest.model.QueryData;
import li.strolch.search.SearchResult;
import li.strolch.search.ValueSearch;
import li.strolch.service.JsonServiceArgument;
import li.strolch.service.api.ServiceHandler;
import li.strolch.service.api.ServiceResult;
import li.strolch.service.privilege.users.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@Path("strolch/privilege/users")
public class PrivilegeUsersService {

	private static final Logger logger = LoggerFactory.getLogger(PrivilegeUsersService.class);

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
			SearchResult<UserRep> result = buildSimpleValueSearch(new ValueSearch<UserRep>(), query, Arrays.asList( //
					UserRep::getUsername, //
					UserRep::getFirstname,  //
					UserRep::getLastname,  //
					userRep -> userRep.getUserState().name(),  //
					UserRep::getRoles)) //
					.search(users) //
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
			JsonArray usersArr = privilegeHandler.queryUsers(cert, queryRep).stream() //
					.sorted(comparing(r -> r.getUsername().toLowerCase())) //
					.collect(JsonArray::new, //
							(array, user) -> array.add(user.accept(visitor)), //
							JsonArray::addAll);

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
			return Response.ok(user.accept(new PrivilegeElementToJsonVisitor()).toString(), MediaType.APPLICATION_JSON)
					.build();
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

		PrivilegeUserResult svcResult = svcHandler.doService(cert, svc, arg);
		return handleServiceResult(svcResult);
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

		PrivilegeUserResult svcResult = svcHandler.doService(cert, svc, arg);
		return handleServiceResult(svcResult);
	}

	@PUT
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{username}/roles")
	public Response updateRolesOnUser(@PathParam("username") String username, String data,
			@Context HttpServletRequest request) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		ServiceHandler svcHandler = RestfulStrolchComponent.getInstance().getComponent(ServiceHandler.class);
		PrivilegeUpdateUserRolesService svc = new PrivilegeUpdateUserRolesService();
		JsonServiceArgument arg = svc.getArgumentInstance();
		arg.objectId = username;
		arg.jsonElement = JsonParser.parseString(data);

		PrivilegeUserResult svcResult = svcHandler.doService(cert, svc, arg);
		return handleServiceResult(svcResult);
	}

	@PUT
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{username}/roles/{rolename}")
	public Response addRoleToUser(@PathParam("username") String username, @PathParam("rolename") String rolename,
			@Context HttpServletRequest request) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		ServiceHandler svcHandler = RestfulStrolchComponent.getInstance().getComponent(ServiceHandler.class);
		PrivilegeAddRoleToUserService svc = new PrivilegeAddRoleToUserService();
		PrivilegeRoleUserNamesArgument arg = new PrivilegeRoleUserNamesArgument();
		arg.username = username;
		arg.rolename = rolename;

		PrivilegeUserResult svcResult = svcHandler.doService(cert, svc, arg);
		return handleServiceResult(svcResult);
	}

	@DELETE
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{username}/roles/{rolename}")
	public Response removeRoleFromUser(@PathParam("username") String username, @PathParam("rolename") String rolename,
			@Context HttpServletRequest request) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		ServiceHandler svcHandler = RestfulStrolchComponent.getInstance().getComponent(ServiceHandler.class);
		PrivilegeRemoveRoleFromUserService svc = new PrivilegeRemoveRoleFromUserService();
		PrivilegeRoleUserNamesArgument arg = new PrivilegeRoleUserNamesArgument();
		arg.username = username;
		arg.rolename = rolename;

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
			return ResponseUtil.toResponse(msg);
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
			locale = new Locale(localeS);
		} catch (Exception e) {
			String msg = MessageFormat.format("Locale {0} is not valid!", localeS);
			return ResponseUtil.toResponse(msg);
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

		try {

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
			if (svcResult.isNok())
				return ResponseUtil.toResponse(svcResult);

			// if user changes their own password, then invalidate the session
			if (cert.getUsername().equals(username)) {
				StrolchSessionHandler sessionHandler = RestfulStrolchComponent.getInstance().getSessionHandler();
				sessionHandler.invalidate(cert);
			}

			return ResponseUtil.toResponse();

		} catch (Exception e) {
			logger.error(e.getMessage(), e);
			String msg = e.getMessage();
			return ResponseUtil.toResponse("Failed to set password: ",
					MessageFormat.format("{0}: {1}", e.getClass().getName(), msg));
		}
	}

	@DELETE
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{username}/password")
	public Response clearUserPassword(@PathParam("username") String username, @Context HttpServletRequest request) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		try {

			ServiceHandler svcHandler = RestfulStrolchComponent.getInstance().getComponent(ServiceHandler.class);
			ClearUserPasswordService svc = new ClearUserPasswordService();
			PrivilegeUserNameArgument arg = svc.getArgumentInstance();
			arg.username = username;

			ServiceResult svcResult = svcHandler.doService(cert, svc, arg);
			if (svcResult.isNok())
				return ResponseUtil.toResponse(svcResult);

			// if user changes their own password, then invalidate the session
			if (cert.getUsername().equals(username)) {
				StrolchSessionHandler sessionHandler = RestfulStrolchComponent.getInstance().getSessionHandler();
				sessionHandler.invalidate(cert);
			}

			return ResponseUtil.toResponse();

		} catch (Exception e) {
			logger.error(e.getMessage(), e);
			String msg = e.getMessage();
			return ResponseUtil.toResponse("Failed to clear password: ",
					MessageFormat.format("{0}: {1}", e.getClass().getName(), msg));
		}
	}

	private Response handleServiceResult(PrivilegeUserResult svcResult) {
		if (svcResult.isOk()) {
			UserRep userRep = svcResult.getUser();
			return Response
					.ok(userRep.accept(new PrivilegeElementToJsonVisitor()).toString(), MediaType.APPLICATION_JSON)
					.build();
		}
		return ResponseUtil.toResponse(svcResult);
	}
}