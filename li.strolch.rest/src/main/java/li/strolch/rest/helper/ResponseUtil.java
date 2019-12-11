package li.strolch.rest.helper;

import static li.strolch.rest.StrolchRestfulConstants.*;
import static li.strolch.utils.helper.ExceptionHelper.getExceptionMessageWithCauses;

import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;

import com.google.gson.Gson;
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import li.strolch.exception.StrolchElementNotFoundException;
import li.strolch.privilege.base.AccessDeniedException;
import li.strolch.privilege.base.PrivilegeException;
import li.strolch.privilege.base.PrivilegeModelException;
import li.strolch.service.JsonServiceResult;
import li.strolch.service.api.ServiceResult;
import li.strolch.utils.collections.Paging;
import li.strolch.utils.helper.StringHelper;

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
		if (t instanceof AccessDeniedException) {
			status = Status.FORBIDDEN;
		} else if (t instanceof PrivilegeModelException) {
			status = Status.INTERNAL_SERVER_ERROR;
		} else if (t instanceof PrivilegeException) {
			status = Status.UNAUTHORIZED;
		} else if (t instanceof StrolchElementNotFoundException) {
			status = Status.NOT_FOUND;
		} else {
			status = Status.INTERNAL_SERVER_ERROR;
		}

		return Response.status(status).entity(json).type(MediaType.APPLICATION_JSON).build();
	}

	public static Response toResponse(Throwable t) {
		if (t instanceof AccessDeniedException) {
			return ResponseUtil.toResponse(Status.FORBIDDEN, t);
		} else if (t instanceof StrolchElementNotFoundException) {
			return ResponseUtil.toResponse(Status.NOT_FOUND, t);
		} else if (t instanceof PrivilegeModelException) {
			return ResponseUtil.toResponse(Status.INTERNAL_SERVER_ERROR, t);
		} else if (t instanceof PrivilegeException) {
			return ResponseUtil.toResponse(Status.UNAUTHORIZED, t);
		} else {
			return toResponse(Status.INTERNAL_SERVER_ERROR, t);
		}
	}

	public static Response toResponse(Status status, String msg) {
		JsonObject response = new JsonObject();
		response.addProperty(MSG, msg);
		String json = new Gson().toJson(response);

		return Response.status(status).entity(json).type(MediaType.APPLICATION_JSON).build();
	}

	public static Response toResponse(Status status, Throwable t) {
		JsonObject response = new JsonObject();
		response.addProperty(MSG, getExceptionMessageWithCauses(t, false));
		String json = new Gson().toJson(response);

		return Response.status(status).entity(json).type(MediaType.APPLICATION_JSON).build();
	}

	public static <T> Response toResponse(Paging<T> paging, Function<T, JsonObject> visitor) {
		JsonObject response = new JsonObject();
		addPagingInfo(paging, response);

		List<T> page = paging.getPage();
		JsonArray data = new JsonArray();
		page.forEach(t -> data.add(visitor.apply(t)));
		response.add(DATA, data);

		String json = new Gson().toJson(response);

		return Response.ok(json, MediaType.APPLICATION_JSON).build();
	}

	public static Response toResponse(Paging<JsonObject> paging) {
		JsonObject response = new JsonObject();
		addPagingInfo(paging, response);

		List<JsonObject> page = paging.getPage();
		JsonArray data = new JsonArray();
		for (JsonObject jsonObject : page) {
			data.add(jsonObject);
		}
		response.add(DATA, data);

		String json = new Gson().toJson(response);

		return Response.ok(json, MediaType.APPLICATION_JSON).build();
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
