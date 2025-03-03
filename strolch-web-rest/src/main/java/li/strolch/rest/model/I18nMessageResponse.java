package li.strolch.rest.model;

import com.google.gson.JsonObject;
import io.swagger.v3.oas.annotations.media.Schema;
import li.strolch.model.Tags;
import li.strolch.utils.I18nMessage;

import java.util.HashMap;
import java.util.Map;

@Schema(description = "Represents an i18n message")
public class I18nMessageResponse {

	@Schema(description = "The key of the i18n message")
	private final String key;
	@Schema(description = "The formatted i18n message")
	private final String message;
	@Schema(description = "The stack trace if available")
	private final String exception;

	@Schema(type = "object")
	private final Map<String, String> values;

	protected I18nMessageResponse(I18nMessage i18nMessage) {
		this.key = i18nMessage.getKey();
		this.message = i18nMessage.getMessage();
		this.exception = i18nMessage.getStackTrace();
		this.values = new HashMap<>();
		i18nMessage.getValues().forEach((o, o2) -> this.values.put(o.toString(), o2.toString()));
	}

	public String getKey() {
		return key;
	}

	public String getMessage() {
		return message;
	}

	public String getException() {
		return exception;
	}

	public Map<String, String> getValues() {
		return values;
	}

	public JsonObject toJson() {
		JsonObject json = new JsonObject();

		json.addProperty(Tags.Json.KEY, this.key);
		json.addProperty(Tags.Json.MESSAGE, this.message);
		json.addProperty(Tags.Json.EXCEPTION, this.exception);

		if (!this.values.isEmpty()) {
			JsonObject valuesJ = new JsonObject();
			this.values.forEach(valuesJ::addProperty);
			json.add(Tags.Json.VALUES, valuesJ);
		}

		return json;
	}
}
