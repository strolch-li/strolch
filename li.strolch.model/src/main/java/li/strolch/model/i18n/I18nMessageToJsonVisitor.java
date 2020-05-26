package li.strolch.model.i18n;

import java.util.Properties;

import com.google.gson.JsonObject;
import li.strolch.model.Tags;
import li.strolch.utils.I18nMessage;
import li.strolch.utils.I18nMessageVisitor;

public class I18nMessageToJsonVisitor implements I18nMessageVisitor<JsonObject> {

	@Override
	public JsonObject visit(I18nMessage message) {
		JsonObject json = new JsonObject();

		json.addProperty(Tags.Json.KEY, message.getKey());
		json.addProperty(Tags.Json.MESSAGE, message.getMessage());

		Properties values = message.getValues();
		if (!values.isEmpty()) {
			JsonObject valuesJ = new JsonObject();
			values.stringPropertyNames().forEach(key -> valuesJ.addProperty(key, values.getProperty(key)));
			json.add(Tags.Json.VALUES, valuesJ);
		}

		return json;
	}
}
