package li.strolch.rest.model;

import com.google.gson.Gson;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import io.swagger.v3.oas.annotations.media.Schema;
import li.strolch.utils.I18nMessage;

import static li.strolch.rest.StrolchRestfulConstants.*;
import static li.strolch.utils.helper.StringHelper.DASH;

@Schema(description = "Represents a standard Strolch response")
public class StrolchResponse {

	@Schema(description = "Message describing the result.", example = "Operation completed successfully.")
	private final String msg;

	@Schema(description = "Internationalization message details if available.", nullable = true)
	private final I18nMessageResponse i18n;

	@Schema(description = "Message describing the result.", example = "Operation completed successfully.",
			nullable = true, type = "object")
	private final JsonElement data;

	public StrolchResponse(String msg, I18nMessageResponse i18n, JsonElement data) {
		this.msg = msg;
		this.i18n = i18n;
		this.data = data;
	}

	public String getMsg() {
		return this.msg;
	}

	public I18nMessageResponse getI18n() {
		return this.i18n;
	}

	public JsonElement getData() {
		return this.data;
	}

	public static StrolchResponse valueOf() {
		return new StrolchResponse(DASH, null, null);
	}

	public static StrolchResponse valueOf(String msg) {
		return new StrolchResponse(msg, null, null);
	}

	public static StrolchResponse valueOf(String msg, I18nMessage i18nMessage) {
		return new StrolchResponse(msg, new I18nMessageResponse(i18nMessage), null);
	}

	public static StrolchResponse valueOf(String msg, JsonElement data) {
		return new StrolchResponse(msg, null, data);
	}

	public static StrolchResponse valueOf(String msg, I18nMessage i18nMessage, JsonElement data) {
		return new StrolchResponse(msg, new I18nMessageResponse(i18nMessage), data);
	}

	public static StrolchResponse valueOf(JsonElement data) {
		return new StrolchResponse(DASH, null, data);
	}

	public String toJson() {
		return new Gson().toJson(toJsonObject());
	}

	public JsonObject toJsonObject() {
		JsonObject response = new JsonObject();
		response.addProperty(MSG, DASH);
		if (this.i18n != null)
			response.add(I18N, this.i18n.toJson());
		if (this.data != null)
			response.add(DATA, this.data);
		return response;
	}
}
