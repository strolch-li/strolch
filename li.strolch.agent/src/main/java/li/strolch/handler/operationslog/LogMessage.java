package li.strolch.handler.operationslog;

import java.util.ResourceBundle;

import com.google.gson.JsonObject;

import li.strolch.agent.api.StrolchAgent;
import li.strolch.model.Locator;
import li.strolch.model.Tags.Json;
import li.strolch.utils.I18nMessage;
import li.strolch.utils.helper.ExceptionHelper;

public class LogMessage extends I18nMessage {

	private final String id;
	private final String realm;
	private final Locator locator;
	private final LogSeverity severity;

	public LogMessage(String realm, Locator locator, LogSeverity logSeverity, ResourceBundle bundle, String key) {
		super(bundle, key);
		this.id = StrolchAgent.getUniqueId();
		this.realm = realm;
		this.locator = locator;
		this.severity = logSeverity;
	}

	public String getId() {
		return this.id;
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

		jsonObject.addProperty(Json.ID, this.id);
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

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((this.id == null) ? 0 : this.id.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		LogMessage other = (LogMessage) obj;
		if (this.id == null) {
			if (other.id != null)
				return false;
		} else if (!this.id.equals(other.id))
			return false;
		return true;
	}
}
