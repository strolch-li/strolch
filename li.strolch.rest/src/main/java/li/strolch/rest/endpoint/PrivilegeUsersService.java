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

import java.text.MessageFormat;
import java.util.Base64;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

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
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.google.gson.JsonPrimitive;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.privilege.base.AccessDeniedException;
import li.strolch.privilege.base.PrivilegeException;
import li.strolch.privilege.handler.PrivilegeHandler;
import li.strolch.privilege.model.Certificate;
import li.strolch.privilege.model.UserRep;
import li.strolch.privilege.model.UserState;
import li.strolch.rest.RestfulStrolchComponent;
import li.strolch.rest.StrolchRestfulConstants;
import li.strolch.rest.StrolchSessionHandler;
import li.strolch.rest.model.Result;
import li.strolch.service.api.ServiceHandler;
import li.strolch.service.api.ServiceResult;
import li.strolch.service.privilege.users.PrivilegeAddRoleToUserService;
import li.strolch.service.privilege.users.PrivilegeAddUserService;
import li.strolch.service.privilege.users.PrivilegeRemoveRoleFromUserService;
import li.strolch.service.privilege.users.PrivilegeRemoveUserService;
import li.strolch.service.privilege.users.PrivilegeRoleUserNamesArgument;
import li.strolch.service.privilege.users.PrivilegeSetUserLocaleArgument;
import li.strolch.service.privilege.users.PrivilegeSetUserLocaleService;
import li.strolch.service.privilege.users.PrivilegeSetUserPasswordArgument;
import li.strolch.service.privilege.users.PrivilegeSetUserPasswordService;
import li.strolch.service.privilege.users.PrivilegeSetUserStateArgument;
import li.strolch.service.privilege.users.PrivilegeSetUserStateService;
import li.strolch.service.privilege.users.PrivilegeUpdateUserService;
import li.strolch.service.privilege.users.PrivilegeUserArgument;
import li.strolch.service.privilege.users.PrivilegeUserNameArgument;
import li.strolch.service.privilege.users.PrivilegeUserResult;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@Path("strolch/privilege/users")
public class PrivilegeUsersService {

	private PrivilegeHandler getPrivilegeHandler() {
		ComponentContainer container = RestfulStrolchComponent.getInstance().getContainer();
		return container.getPrivilegeHandler().getPrivilegeHandler();
	}

	private JsonArray toJson(List<UserRep> users) {
		JsonArray usersArr = new JsonArray();
		for (UserRep userRep : users) {
			usersArr.add(toJson(userRep));
		}
		return usersArr;
	}

	private JsonObject toJson(UserRep userRep) {
		JsonObject jsonObject = new JsonObject();

		jsonObject.addProperty("userId", userRep.getUserId());
		jsonObject.addProperty("username", userRep.getUsername());
		jsonObject.addProperty("firstname", userRep.getFirstname());
		jsonObject.addProperty("lastname", userRep.getLastname());
		jsonObject.addProperty("userState", userRep.getUserState().name());
		jsonObject.addProperty("locale", userRep.getLocale().toString());

		JsonArray rolesArr = new JsonArray();
		jsonObject.add("roles", rolesArr);
		for (String role : userRep.getRoles()) {
			rolesArr.add(new JsonPrimitive(role));
		}

		JsonArray propsArr = new JsonArray();
		jsonObject.add("properties", propsArr);
		for (String propKey : userRep.getPropertyKeySet()) {
			JsonObject propObj = new JsonObject();
			propObj.addProperty("key", propKey);
			propObj.addProperty("value", userRep.getProperty(propKey));
			propsArr.add(propObj);
		}

		return jsonObject;
	}

	private UserRep fromJson(JsonObject jsonObject) {

		JsonElement userIdE = jsonObject.get("userId");
		JsonElement usernameE = jsonObject.get("username");
		JsonElement firstnameE = jsonObject.get("firstname");
		JsonElement lastnameE = jsonObject.get("lastname");
		JsonElement userStateE = jsonObject.get("userState");
		JsonElement localeE = jsonObject.get("locale");
		JsonElement rolesE = jsonObject.get("roles");
		JsonElement propertiesE = jsonObject.get("properties");

		String userId = userIdE == null ? null : userIdE.getAsString();
		String username = usernameE == null ? null : usernameE.getAsString();
		String firstname = firstnameE == null ? null : firstnameE.getAsString();
		String lastname = lastnameE == null ? null : lastnameE.getAsString();
		UserState userState = userStateE == null ? null : UserState.valueOf(userStateE.getAsString());
		Locale locale = localeE == null ? null : new Locale(localeE.getAsString());

		Set<String> roles = null;
		if (rolesE != null) {
			roles = new HashSet<>();
			JsonArray rolesArr = rolesE.getAsJsonArray();
			for (JsonElement role : rolesArr) {
				roles.add(role.getAsString());
			}
		}

		Map<String, String> properties = null;
		if (propertiesE != null) {
			properties = new HashMap<>();
			JsonArray propertiesArr = propertiesE.getAsJsonArray();
			for (JsonElement propertyE : propertiesArr) {
				JsonObject property = propertyE.getAsJsonObject();
				properties.put(property.get("key").getAsString(), property.get("value").getAsString());
			}
		}

		UserRep userRep = new UserRep(userId, username, firstname, lastname, userState, roles, locale, properties);
		return userRep;
	}

	@GET
	@Produces(MediaType.APPLICATION_JSON)
	public Response getUsers(@Context HttpServletRequest request) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);
		PrivilegeHandler privilegeHandler = getPrivilegeHandler();

		List<UserRep> users = privilegeHandler.getUsers(cert);
		JsonArray usersArr = toJson(users);
		return Response.ok(usersArr.toString(), MediaType.APPLICATION_JSON).build();
	}

	@GET
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{username}")
	public Response getUser(@PathParam("username") String username, @Context HttpServletRequest request) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);
		PrivilegeHandler privilegeHandler = getPrivilegeHandler();

		UserRep user = privilegeHandler.getUser(cert, username);
		return Response.ok(toJson(user).toString(), MediaType.APPLICATION_JSON).build();
	}

	@POST
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)
	@Path("query")
	public Response queryUsers(String query, @Context HttpServletRequest request) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);
		PrivilegeHandler privilegeHandler = getPrivilegeHandler();

		UserRep queryRep = fromJson(new JsonParser().parse(query).getAsJsonObject());
		List<UserRep> users = privilegeHandler.queryUsers(cert, queryRep);
		JsonArray usersArr = toJson(users);
		return Response.ok(usersArr.toString(), MediaType.APPLICATION_JSON).build();
	}

	@POST
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)
	public Response addUser(String newUser, @Context HttpServletRequest request) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		ServiceHandler svcHandler = RestfulStrolchComponent.getInstance().getComponent(ServiceHandler.class);
		PrivilegeAddUserService svc = new PrivilegeAddUserService();
		PrivilegeUserArgument arg = new PrivilegeUserArgument();
		arg.user = fromJson(new JsonParser().parse(newUser).getAsJsonObject());

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
		arg.user = fromJson(new JsonParser().parse(updatedFields).getAsJsonObject());

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
			return Response.serverError().entity(new Result(msg)).type(MediaType.APPLICATION_JSON).build();
		}

		ServiceHandler svcHandler = RestfulStrolchComponent.getInstance().getComponent(ServiceHandler.class);
		PrivilegeSetUserStateService svc = new PrivilegeSetUserStateService();
		PrivilegeSetUserStateArgument arg = new PrivilegeSetUserStateArgument();
		arg.username = username;
		arg.userState = userState;

		PrivilegeUserResult svcResult = svcHandler.doService(cert, svc, arg);
		return handleServiceResult(svcResult);
	}

	private Response handleServiceResult(PrivilegeUserResult svcResult) {
		if (svcResult.isOk()) {
			return Response.ok(toJson(svcResult.getUser()), MediaType.APPLICATION_JSON).build();
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
			return Response.serverError().entity(new Result(msg)).type(MediaType.APPLICATION_JSON).build();
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

		String password = new JsonParser().parse(data).getAsJsonObject().get("password").getAsString();

		ServiceHandler svcHandler = RestfulStrolchComponent.getInstance().getComponent(ServiceHandler.class);
		PrivilegeSetUserPasswordService svc = new PrivilegeSetUserPasswordService();
		PrivilegeSetUserPasswordArgument arg = new PrivilegeSetUserPasswordArgument();
		arg.username = username;
		arg.password = new String(Base64.getDecoder().decode(password)).toCharArray();

		ServiceResult svcResult = svcHandler.doService(cert, svc, arg);
		if (svcResult.isOk()) {

			// if user changes their own password, then invalidate the session
			if (cert.getUsername().equals(username)) {
				StrolchSessionHandler sessionHandler = RestfulStrolchComponent.getInstance().getSessionHandler();
				sessionHandler.invalidate(cert);
			}

			// TODO invalidate any other sessions for this user

			return Response.ok(new Result(), MediaType.APPLICATION_JSON).build();

		} else if (svcResult.getThrowable() != null) {
			Throwable t = svcResult.getThrowable();
			if (t instanceof AccessDeniedException) {
				return Response.status(Status.UNAUTHORIZED).entity(new Result(t.getMessage()))
						.type(MediaType.APPLICATION_JSON).build();
			} else if (t instanceof PrivilegeException) {
				return Response.status(Status.FORBIDDEN).entity(new Result(t.getMessage())).build();
			}
		}

		return Response.status(Status.INTERNAL_SERVER_ERROR).entity(new Result(svcResult.getMessage())).build();
	}
}