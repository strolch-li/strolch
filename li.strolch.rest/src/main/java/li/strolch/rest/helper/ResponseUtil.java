package li.strolch.rest.helper;

import static li.strolch.rest.StrolchRestfulConstants.DATA;
import static li.strolch.rest.StrolchRestfulConstants.EXCEPTION_MSG;
import static li.strolch.rest.StrolchRestfulConstants.LAST_OFFSET;
import static li.strolch.rest.StrolchRestfulConstants.LIMIT;
import static li.strolch.rest.StrolchRestfulConstants.MSG;
import static li.strolch.rest.StrolchRestfulConstants.NEXT_OFFSET;
import static li.strolch.rest.StrolchRestfulConstants.OFFSET;
import static li.strolch.rest.StrolchRestfulConstants.PREVIOUS_OFFSET;
import static li.strolch.rest.StrolchRestfulConstants.SIZE;

import java.util.List;

import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;

import com.google.gson.Gson;
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;

import li.strolch.service.api.ServiceResult;
import li.strolch.utils.collections.Paging;
import li.strolch.utils.helper.ExceptionHelper;
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

	public static Response toResponse(String member, JsonElement jsonElement) {
		JsonObject response = new JsonObject();
		response.addProperty(MSG, StringHelper.DASH);
		response.add(member, jsonElement);

		String json = new Gson().toJson(response);

		return Response.ok(json, MediaType.APPLICATION_JSON).build();
	}

	public static Response toResponse(String member, List<JsonObject> jsonObjects) {
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

	public static Response toResponse(ServiceResult svcResult) {

		String msg = StringHelper.DASH;
		String exceptionMsg = StringHelper.DASH;

		if (!svcResult.isOk()) {
			msg = svcResult.getMessage();
			Throwable t = svcResult.getThrowable();
			if (t != null)
				exceptionMsg = StringHelper.formatExceptionMessage(t);
		}

		JsonObject response = new JsonObject();
		response.addProperty(MSG, msg);
		if (!exceptionMsg.equals(StringHelper.DASH))
			response.addProperty(EXCEPTION_MSG, exceptionMsg);

		String json = new Gson().toJson(response);
		return Response.serverError().entity(json).type(MediaType.APPLICATION_JSON).build();
	}

	public static Response toResponse(Throwable t) {
		return toResponse(Status.INTERNAL_SERVER_ERROR, t);
	}

	public static Response toResponse(Status status, Throwable t) {
		JsonObject response = new JsonObject();
		response.addProperty(MSG, ExceptionHelper.getExceptionMessageWithCauses(t));
		String json = new Gson().toJson(response);

		return Response.status(status).entity(json).type(MediaType.APPLICATION_JSON).build();
	}

	public static Response toResponse(Paging<JsonObject> paging) {
		JsonObject response = new JsonObject();

		response.addProperty(MSG, StringHelper.DASH);

		response.addProperty(LIMIT, paging.getLimit());
		response.addProperty(OFFSET, paging.getOffset());
		response.addProperty(SIZE, paging.getSize());
		response.addProperty(PREVIOUS_OFFSET, paging.getPreviousOffset());
		response.addProperty(NEXT_OFFSET, paging.getNextOffset());
		response.addProperty(LAST_OFFSET, paging.getLastOffset());

		List<JsonObject> page = paging.getPage();
		JsonArray data = new JsonArray();
		for (JsonObject jsonObject : page) {
			JsonObject element = jsonObject;
			data.add(element);
		}
		response.add(DATA, data);

		String json = new Gson().toJson(response);

		return Response.ok(json, MediaType.APPLICATION_JSON).build();
	}

}
