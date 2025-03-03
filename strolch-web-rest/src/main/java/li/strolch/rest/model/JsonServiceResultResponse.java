package li.strolch.rest.model;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import io.swagger.v3.oas.annotations.media.Schema;
import li.strolch.service.JsonServiceResult;

import static li.strolch.model.Tags.Json.DATA;

@Schema(description = "Represents the result of a service execution which returns JSON data")
public class JsonServiceResultResponse extends ServiceResultResponse {

	@Schema(description = "Data returned by the service for the consumer", nullable = true, type = "object")
	private final JsonElement data;

	public JsonServiceResultResponse(JsonServiceResult serviceResult) {
		super(serviceResult);
		this.data = serviceResult.getResult();
	}

	public JsonElement getData() {
		return this.data;
	}

	@Override
	public JsonObject toJsonObject() {
		JsonObject json = super.toJsonObject();
		json.add(DATA, this.data);
		return json;
	}
}
