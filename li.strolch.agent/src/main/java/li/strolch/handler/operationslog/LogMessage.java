package li.strolch.handler.operationslog;

import java.util.ResourceBundle;

import com.google.gson.JsonObject;

import li.strolch.model.Locator;
import li.strolch.model.Tags.Json;
import li.strolch.utils.I18nMessage;
import li.strolch.utils.helper.ExceptionHelper;

public class LogMessage extends I18nMessage {

	private final String realm;
	private final Locator locator;
	private final LogSeverity severity;

	public LogMessage(String realm, Locator locator, LogSeverity logSeverity, ResourceBundle bundle, String key) {
		super(bundle, key);
		this.realm = realm;
		this.locator = locator;
		this.severity = logSeverity;
	}

	public String getRealm() {
		return this.realm;
	}

	public Locator getLocator() {
		return this.locator;
	}

	public LogSeverity getSeverity() {
		return this.severity;
	}

	@Override
	public LogMessage value(String key, String value) {
		super.value(key, value);
		return this;
	}

	public LogMessage value(String key, Throwable e) {
		super.value(key, ExceptionHelper.getExceptionMessageWithCauses(e));
		return this;
	}

	public JsonObject toJson() {

		JsonObject jsonObject = new JsonObject();

		jsonObject.addProperty(Json.KEY, getKey());
		jsonObject.addProperty(Json.MESSAGE, formatMessage());
		jsonObject.addProperty(Json.SEVERITY, this.severity.getSeverity());
		jsonObject.addProperty(Json.REALM, this.realm);
		jsonObject.addProperty(Json.LOCATOR, this.locator.toString());
		JsonObject values = new JsonObject();
		for (String key : getValues().stringPropertyNames()) {
			values.addProperty(key, getValues().getProperty(key));
		}
		jsonObject.add(Json.VALUES, values);

		return jsonObject;
	}
}
