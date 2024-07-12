package li.strolch.rest.helper;

import com.google.gson.Gson;
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import jakarta.ws.rs.core.MediaType;
import jakarta.ws.rs.core.Response;
import jakarta.ws.rs.core.Response.Status;
import li.strolch.exception.StrolchElementNotFoundException;
import li.strolch.exception.StrolchException;
import li.strolch.exception.StrolchNotAuthenticatedException;
import li.strolch.exception.StrolchUserMessageException;
import li.strolch.model.i18n.I18nMessageToJsonVisitor;
import li.strolch.privilege.base.AccessDeniedException;
import li.strolch.privilege.base.PrivilegeException;
import li.strolch.privilege.base.PrivilegeModelException;
import li.strolch.service.JsonServiceResult;
import li.strolch.service.api.ServiceResult;
import li.strolch.utils.I18nMessage;
import li.strolch.utils.collections.Paging;
import li.strolch.utils.helper.ExceptionHelper;
import li.strolch.utils.helper.StringHelper;

import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;

import static li.strolch.rest.StrolchRestfulConstants.*;
import static li.strolch.utils.helper.ExceptionHelper.getExceptionMessageWithCauses;
import static li.strolch.utils.helper.ExceptionHelper.getRootCause;

/**
 * Created by eitch on 29.08.16.
 */
public class ResponseUtil {

	public static Response toResponse() {
		JsonObject response = new JsonObject();
		response.addProperty(MSG, StringHelper.DASH);
		String json = new Gson().toJson(response);
		return Response.ok(json, MediaType.APPLICATION_JSON).build();
	}

	public static Response toResponse(Status status, I18nMessage msg) {
		JsonObject response = new JsonObject();
		response.addProperty(MSG, msg.getMessage());
		response.add(I18N, msg.accept(new I18nMessageToJsonVisitor()));
		String json = new Gson().toJson(response);
		return Response.status(status).entity(json).type(MediaType.APPLICATION_JSON).build();
	}

	public static Response toResponse(String errorMsg) {
		JsonObject response = new JsonObject();
		response.addProperty(MSG, errorMsg);
		String json = new Gson().toJson(response);
		return Response.serverError().entity(json).type(MediaType.APPLICATION_JSON).build();
	}

	public static Response toResponse(String prop, String value) {
		JsonObject response = new JsonObject();
		response.addProperty(MSG, StringHelper.DASH);
		response.addProperty(prop, value);
		String json = new Gson().toJson(response);
		return Response.ok(json, MediaType.APPLICATION_JSON).build();
	}

	public static Response toResponse(String prop1, String value1, String prop2, String value2) {
		JsonObject response = new JsonObject();
		response.addProperty(MSG, StringHelper.DASH);
		response.addProperty(prop1, value1);
		response.addProperty(prop2, value2);
		String json = new Gson().toJson(response);
		return Response.ok(json, MediaType.APPLICATION_JSON).build();
	}

	public static Response toResponse(String prop1, String value1, JsonArray data) {
		JsonObject response = new JsonObject();
		response.addProperty(MSG, StringHelper.DASH);
		response.addProperty(prop1, value1);
		response.add(DATA, data);
		String json = new Gson().toJson(response);
		return Response.ok(json, MediaType.APPLICATION_JSON).build();
	}

	public static <T> Response toResponse(String member, T t, Function<T, JsonObject> toJson) {
		return toResponse(member, toJson.apply(t));
	}

	public static Response toResponse(String member, JsonElement jsonElement) {
		JsonObject response = new JsonObject();
		response.addProperty(MSG, StringHelper.DASH);
		response.add(member, jsonElement);
		String json = new Gson().toJson(response);
		return Response.ok(json, MediaType.APPLICATION_JSON).build();
	}

	public static Response toResponse(JsonObject response) {
		response.addProperty(MSG, StringHelper.DASH);
		String json = new Gson().toJson(response);
		return Response.ok(json, MediaType.APPLICATION_JSON).build();
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
		JsonObject response = new JsonObject();
		response.addProperty(MSG, StringHelper.DASH);

		JsonArray arrayJ = new JsonArray();
		for (JsonElement obj : jsonObjects) {
			arrayJ.add(obj);
		}
		response.add(member, arrayJ);

		String json = new Gson().toJson(response);
		return Response.ok(json, MediaType.APPLICATION_JSON).build();
	}

	public static Response toResponse(JsonServiceResult svcResult) {
		if (svcResult.isOk())
			return toResponse(DATA, svcResult.getResult());
		return toResponse((ServiceResult) svcResult);
	}

	public static Response toResponse(ServiceResult svcResult) {
		Throwable t = svcResult.getThrowable();
		JsonObject response = svcResult.toJson();
		String json = new Gson().toJson(response);

		if (svcResult.isOk())
			return Response.ok().entity(json).type(MediaType.APPLICATION_JSON).build();

		Status status;
		if (t == null) {
			status = Status.INTERNAL_SERVER_ERROR;
		} else {
			Throwable rootCause = getRootCause(t);
			status = switch (rootCause) {
				case AccessDeniedException ignored -> Status.FORBIDDEN;
				case PrivilegeException ignored -> Status.UNAUTHORIZED;
				case StrolchElementNotFoundException ignored -> Status.NOT_FOUND;
				case null, default -> Status.INTERNAL_SERVER_ERROR;
			};
		}

		return Response.status(status).entity(json).type(MediaType.APPLICATION_JSON).build();
	}

	public static Response toResponse(Throwable t) {
		Throwable rootCause = getRootCause(t);
		return switch (rootCause) {
			case StrolchNotAuthenticatedException ignored -> toResponse(Status.UNAUTHORIZED, rootCause);
			case AccessDeniedException ignored -> toResponse(Status.FORBIDDEN, rootCause);
			case StrolchElementNotFoundException ignored -> toResponse(Status.NOT_FOUND, rootCause);
			case PrivilegeException ignored -> toResponse(Status.FORBIDDEN, rootCause);
			case null, default -> toResponse(Status.INTERNAL_SERVER_ERROR, rootCause);
		};
	}

	public static Response toResponse(Status status, String msg) {
		JsonObject response = new JsonObject();
		response.addProperty(MSG, msg);
		String json = new Gson().toJson(response);

		return Response.status(status).entity(json).type(MediaType.APPLICATION_JSON).build();
	}

	public static Response toResponse(Status status, Throwable t) {
		JsonObject response = new JsonObject();

		switch (t) {
			case StrolchException ex when ex.hasI18n() ->
					response.add("i18n", ex.getI18n().accept(new I18nMessageToJsonVisitor()));
			case null, default -> {
				Throwable rootCause = getRootCause(t);
				if (rootCause instanceof StrolchUserMessageException ex && ex.hasI18n())
					response.add("i18n", ex.getI18n().accept(new I18nMessageToJsonVisitor()));
			}
		}

		response.addProperty(MSG, getExceptionMessageWithCauses(t, false));
		String json = new Gson().toJson(response);
		return Response.status(status).entity(json).type(MediaType.APPLICATION_JSON).build();
	}

	public static <T> Response toResponse(List<T> list, Function<T, JsonObject> visitor) {
		JsonObject response = new JsonObject();
		response.addProperty(MSG, StringHelper.DASH);
		return handleIterable(visitor, response, list);
	}

	public static <T> Response toResponse(Paging<T> paging, Function<T, JsonObject> visitor) {
		JsonObject response = new JsonObject();
		addPagingInfo(paging, response);
		return handleIterable(visitor, response, paging.getPage());
	}

	public static Response toResponse(Paging<JsonObject> paging) {
		JsonObject response = new JsonObject();
		addPagingInfo(paging, response);
		return handleIterable(e -> e, response, paging.getPage());
	}

	private static <T> Response handleIterable(Function<T, JsonObject> visitor, JsonObject response, Iterable<T> page) {
		JsonArray data = new JsonArray();
		page.forEach(t -> data.add(visitor.apply(t)));
		response.add(DATA, data);
		return Response.ok(new Gson().toJson(response), MediaType.APPLICATION_JSON).build();
	}

	private static <T> void addPagingInfo(Paging<T> paging, JsonObject response) {
		response.addProperty(MSG, StringHelper.DASH);

		response.addProperty(DATA_SET_SIZE, paging.getDataSetSize());
		response.addProperty(LIMIT, paging.getLimit());
		response.addProperty(OFFSET, paging.getOffset());
		response.addProperty(SIZE, paging.getSize());
		response.addProperty(PREVIOUS_OFFSET, paging.getPreviousOffset());
		response.addProperty(NEXT_OFFSET, paging.getNextOffset());
		response.addProperty(LAST_OFFSET, paging.getLastOffset());
	}
}
