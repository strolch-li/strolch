package li.strolch.rest.helper;

import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import com.google.gson.Gson;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;

import li.strolch.utils.helper.ExceptionHelper;
import li.strolch.utils.helper.StringHelper;

/**
 * Created by eitch on 29.08.16.
 */
public class ResponseUtil {

	public static final String MSG = "msg";

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

	public static Response toResponse(String member, JsonElement jsonElement) {
		JsonObject response = new JsonObject();
		response.addProperty(MSG, StringHelper.DASH);
		response.add(member, jsonElement);

		String json = new Gson().toJson(response);

		return Response.ok(json, MediaType.APPLICATION_JSON).build();
	}

	public static Response toResponse(Throwable t) {
		JsonObject response = new JsonObject();
		response.addProperty(MSG, ExceptionHelper.getExceptionMessageWithCauses(t));
		String json = new Gson().toJson(response);

		return Response.serverError().entity(json).type(MediaType.APPLICATION_JSON).build();
	}
}
