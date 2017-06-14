package li.strolch.handler.operationslog;

import com.google.gson.JsonObject;

import li.strolch.model.Locator;
import li.strolch.model.Tags.Json;
import li.strolch.utils.I18nMessage;

public class LogMessage extends I18nMessage {

	private final Locator locator;
	private final LogSeverity severity;

	public LogMessage(Locator locator, LogSeverity logSeverity, String bundleId, String key) {
		super(bundleId, key);
		this.locator = locator;
		this.severity = logSeverity;
	}

	public LogSeverity getSeverity() {
		return this.severity;
	}

	public Locator getLocator() {
		return this.locator;
	}

	@Override
	public LogMessage value(String key, String value) {
		super.value(key, value);
		return this;
	}

	public JsonObject toJson() {

		JsonObject jsonObject = new JsonObject();

		jsonObject.addProperty(Json.KEY, getKey());
		jsonObject.addProperty(Json.MESSAGE, formatMessage());
		jsonObject.addProperty(Json.SEVERITY, this.severity.getSeverity());
		jsonObject.addProperty(Json.LOCATOR, this.locator.toString());
		JsonObject values = new JsonObject();
		for (String key : getValues().stringPropertyNames()) {
			values.addProperty(key, getValues().getProperty(key));
		}
		jsonObject.add(Json.VALUES, values);

		return jsonObject;
	}
}
