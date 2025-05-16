/*
 * Copyright (c) 2013-2025 Robert von Burg <eitch@eitchnet.ch>
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

package li.strolch.rest.helper;

import com.google.gson.Gson;
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import jakarta.ws.rs.core.Response;
import jakarta.ws.rs.core.Response.Status;
import li.strolch.exception.StrolchAccessDeniedException;
import li.strolch.exception.StrolchElementNotFoundException;
import li.strolch.exception.StrolchException;
import li.strolch.exception.StrolchUserMessageException;
import li.strolch.privilege.base.AccessDeniedException;
import li.strolch.privilege.base.NotAuthenticatedException;
import li.strolch.privilege.base.PrivilegeException;
import li.strolch.privilege.base.PrivilegeModelException;
import li.strolch.rest.model.JsonServiceResultResponse;
import li.strolch.rest.model.PagingResponse;
import li.strolch.rest.model.ServiceResultResponse;
import li.strolch.rest.model.StrolchResponse;
import li.strolch.service.JsonServiceResult;
import li.strolch.service.api.ServiceResult;
import li.strolch.utils.I18nMessage;
import li.strolch.utils.collections.Paging;
import li.strolch.utils.helper.StringHelper;

import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;

import static jakarta.ws.rs.core.MediaType.APPLICATION_JSON;
import static li.strolch.rest.StrolchRestfulConstants.DATA;
import static li.strolch.rest.StrolchRestfulConstants.MSG;
import static li.strolch.utils.helper.ExceptionHelper.getExceptionMessageWithCauses;
import static li.strolch.utils.helper.ExceptionHelper.getRootCause;

/**
 * Created by eitch on 29.08.16.
 */
public class ResponseUtil {

	public static Response toResponse() {
		return Response.ok(StrolchResponse.valueOf().toJson(), APPLICATION_JSON).build();
	}

	public static Response toResponse(Status status, I18nMessage msg) {
		String json = StrolchResponse.valueOf(msg.getMessage()).toJson();
		return Response.status(status).entity(json).type(APPLICATION_JSON).build();
	}

	public static Response toResponse(String errorMsg) {
		String json = StrolchResponse.valueOf(errorMsg).toJson();
		return Response.serverError().entity(json).type(APPLICATION_JSON).build();
	}

	public static Response toResponse(String prop, String value) {
		JsonObject response = StrolchResponse.valueOf().toJsonObject();
		response.addProperty(prop, value);
		String json = new Gson().toJson(response);
		return Response.ok(json, APPLICATION_JSON).build();
	}

	public static Response toResponse(String prop1, String value1, String prop2, String value2) {
		JsonObject response = StrolchResponse.valueOf().toJsonObject();
		response.addProperty(prop1, value1);
		response.addProperty(prop2, value2);
		String json = new Gson().toJson(response);
		return Response.ok(json, APPLICATION_JSON).build();
	}

	public static Response toResponse(String prop1, String value1, JsonArray data) {
		JsonObject response = StrolchResponse.valueOf().toJsonObject();
		response.addProperty(prop1, value1);
		response.add(DATA, data);
		String json = new Gson().toJson(response);
		return Response.ok(json, APPLICATION_JSON).build();
	}

	public static <T> Response toResponse(String member, T t, Function<T, JsonObject> toJson) {
		return toResponse(member, toJson.apply(t));
	}

	public static Response toResponse(String member, JsonElement jsonElement) {
		JsonObject response = StrolchResponse.valueOf().toJsonObject();
		response.add(member, jsonElement);
		String json = new Gson().toJson(response);
		return Response.ok(json, APPLICATION_JSON).build();
	}

	public static Response toResponse(JsonObject response) {
		response.addProperty(MSG, StringHelper.DASH);
		String json = new Gson().toJson(response);
		return Response.ok(json, APPLICATION_JSON).build();
	}

	public static <T> Response toResponse(String member, List<T> list, Function<T, JsonObject> toJson) {
		return toResponse(member, list.stream().map(toJson).collect(Collectors.toList()));
	}

	public static <T> Response listToResponse(String member, List<T> list, Function<T, JsonObject> toJson) {
		return toResponse(member, list.stream().map(toJson).collect(Collectors.toList()));
	}

	public static Response toResponse(List<? extends JsonElement> jsonObjects) {
		return toResponse(DATA, jsonObjects);
	}

	public static Response toResponse(String member, List<? extends JsonElement> jsonObjects) {
		JsonObject response = StrolchResponse.valueOf().toJsonObject();

		JsonArray arrayJ = new JsonArray();
		for (JsonElement obj : jsonObjects) {
			arrayJ.add(obj);
		}
		response.add(member, arrayJ);

		String json = new Gson().toJson(response);
		return Response.ok(json, APPLICATION_JSON).build();
	}

	public static Response toResponse(JsonServiceResult svcResult) {
		if (svcResult.isOk())
			return Response.ok(new JsonServiceResultResponse(svcResult).toJson(), APPLICATION_JSON).build();
		return toResponse((ServiceResult) svcResult);
	}

	public static Response toResponse(ServiceResult svcResult) {
		Throwable t = svcResult.getThrowable();
		ServiceResultResponse response = new ServiceResultResponse(svcResult);
		String json = new Gson().toJson(response);

		if (svcResult.isOk())
			return Response.ok().entity(json).type(APPLICATION_JSON).build();

		Status status = evaluateStatus(t);
		return Response.status(status).entity(json).type(APPLICATION_JSON).build();
	}

	public static Response toResponse(Throwable t) {
		Status status = evaluateStatus(t);
		Throwable rootCause = getRootCause(t);
		return toResponse(status, rootCause);
	}

	public static Response toResponse(Status status, String msg) {
		String json = StrolchResponse.valueOf(msg).toJson();
		return Response.status(status).entity(json).type(APPLICATION_JSON).build();
	}

	public static Response toResponse(Status status, Throwable t) {
		I18nMessage i18nMessage = evaluateI18nMessage(t);
		String msg = getExceptionMessageWithCauses(t, false);
		String json = (
				i18nMessage == null ? StrolchResponse.valueOf(msg) :
						StrolchResponse.valueOf(msg, i18nMessage)).toJson();
		return Response.status(status).entity(json).type(APPLICATION_JSON).build();
	}

	public static <T> Response toResponse(List<T> list, Function<T, JsonObject> visitor) {
		JsonObject response = StrolchResponse.valueOf().toJsonObject();
		return handleIterable(visitor, response, list);
	}

	public static <T> Response toResponse(Paging<T> paging, Function<T, JsonObject> visitor) {
		JsonObject response = StrolchResponse.valueOf().toJsonObject();
		new PagingResponse<>(paging).addPagingInfo(response);
		return handleIterable(visitor, response, paging.getPage());
	}

	public static Response toResponse(Paging<JsonObject> paging) {
		JsonObject response = StrolchResponse.valueOf().toJsonObject();
		new PagingResponse<>(paging).addPagingInfo(response);
		return handleIterable(e -> e, response, paging.getPage());
	}

	private static <T> Response handleIterable(Function<T, JsonObject> visitor, JsonObject response, Iterable<T> page) {
		JsonArray data = new JsonArray();
		page.forEach(t -> data.add(visitor.apply(t)));
		response.add(DATA, data);
		return Response.ok(new Gson().toJson(response), APPLICATION_JSON).build();
	}

	private static Status evaluateStatus(Throwable throwable) {
		Throwable rootCause = throwable == null ? null : getRootCause(throwable);
		return switch (rootCause) {
			case AccessDeniedException ignored -> Status.FORBIDDEN;
			case StrolchAccessDeniedException ignored -> Status.FORBIDDEN;
			case PrivilegeModelException ignored -> Status.FORBIDDEN;
			case NotAuthenticatedException ignored -> Status.UNAUTHORIZED;
			case PrivilegeException ignored -> Status.UNAUTHORIZED;
			case StrolchElementNotFoundException ignored -> Status.NOT_FOUND;
			case null, default -> Status.INTERNAL_SERVER_ERROR;
		};
	}

	private static I18nMessage evaluateI18nMessage(Throwable t) {
		return switch (t) {
			case StrolchException ex when ex.hasI18n() -> ex.getI18n();
			case null, default -> {
				Throwable rootCause = t == null ? null : getRootCause(t);
				if (rootCause instanceof StrolchUserMessageException ex && ex.hasI18n())
					yield ex.getI18n();
				yield null;
			}
		};
	}
}
