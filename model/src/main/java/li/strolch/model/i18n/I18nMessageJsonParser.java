package li.strolch.model.i18n;

import java.util.Properties;
import java.util.Set;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import li.strolch.model.Tags;
import li.strolch.utils.I18nMessage;

public class I18nMessageJsonParser {

	public I18nMessage parse(JsonObject messageJ) {

		String key = messageJ.get(Tags.Json.KEY).getAsString();
		String bundle = messageJ.get(Tags.Json.BUNDLE).getAsString();
		String message = messageJ.get(Tags.Json.MESSAGE).getAsString();

		Properties properties = new Properties();
		if (messageJ.has(Tags.Json.VALUES)) {
			JsonArray valuesJ = messageJ.getAsJsonArray(Tags.Json.VALUES);
			for (JsonElement jsonElement : valuesJ) {
				JsonObject valueJ = jsonElement.getAsJsonObject();

				Set<String> keys = valueJ.keySet();
				for (String propertyName : keys) {
					properties.setProperty(propertyName, valueJ.get(propertyName).getAsString());
				}
			}
		}

		return new I18nMessage(bundle, key, properties, message);
	}
}
