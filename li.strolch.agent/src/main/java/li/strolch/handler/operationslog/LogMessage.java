package li.strolch.handler.operationslog;

import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Properties;
import java.util.ResourceBundle;

import com.google.gson.JsonObject;
import li.strolch.agent.api.StrolchAgent;
import li.strolch.model.Locator;
import li.strolch.model.Tags.Json;
import li.strolch.utils.I18nMessage;
import li.strolch.utils.helper.ExceptionHelper;

public class LogMessage extends I18nMessage {

	private final String id;
	private final String username;
	private ZonedDateTime zonedDateTime;
	private final String realm;
	private final Locator locator;
	private final LogSeverity severity;
	private String stackTrace;

	public LogMessage(String realm, String username, Locator locator, LogSeverity severity, ResourceBundle bundle,
			String key) {
		super(bundle, key);
		this.id = StrolchAgent.getUniqueId();
		this.zonedDateTime = ZonedDateTime.now();
		// persisting in the DB only handles millisecond precision, not nano precision
		this.zonedDateTime = this.zonedDateTime.withNano((this.zonedDateTime.getNano() / 1000000) * 1000000);
		this.realm = realm;
		this.username = username;
		this.locator = locator;
		this.severity = severity;
	}

	public LogMessage(String id, ZonedDateTime zonedDateTime, String realm, String username, Locator locator,
			LogSeverity severity, String key, Properties values, String message, String stackTrace) {
		super(key, values, message);
		this.id = id;
		this.zonedDateTime = zonedDateTime;
		this.realm = realm;
		this.username = username;
		this.locator = locator;
		this.severity = severity;
		this.stackTrace = stackTrace;
	}

	public String getId() {
		return this.id;
	}

	public ZonedDateTime getZonedDateTime() {
		return this.zonedDateTime;
	}

	public String getRealm() {
		return this.realm;
	}

	public String getUsername() {
		return this.username;
	}

	public Locator getLocator() {
		return this.locator;
	}

	public LogSeverity getSeverity() {
		return this.severity;
	}

	public LogMessage withException(Throwable t) {
		this.stackTrace = ExceptionHelper.formatException(t);
		return this;
	}

	public String getStackTrace() {
		return this.stackTrace;
	}

	@Override
	public LogMessage value(String key, Object value) {
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
		jsonObject.addProperty(Json.DATE, this.zonedDateTime.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME));
		jsonObject.addProperty(Json.KEY, getKey());
		jsonObject.addProperty(Json.MESSAGE, formatMessage());
		jsonObject.addProperty(Json.SEVERITY, this.severity.name());
		jsonObject.addProperty(Json.USERNAME, this.username);
		jsonObject.addProperty(Json.REALM, this.realm);
		jsonObject.addProperty(Json.LOCATOR, this.locator.toString());
		jsonObject.addProperty(Json.EXCEPTION, this.stackTrace);
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
		int result = super.hashCode();
		result = prime * result + ((this.locator == null) ? 0 : this.locator.hashCode());
		result = prime * result + ((this.realm == null) ? 0 : this.realm.hashCode());
		result = prime * result + ((this.severity == null) ? 0 : this.severity.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (!super.equals(obj))
			return false;
		if (getClass() != obj.getClass())
			return false;
		LogMessage other = (LogMessage) obj;
		if (this.locator == null) {
			if (other.locator != null)
				return false;
		} else if (!this.locator.equals(other.locator))
			return false;
		if (this.realm == null) {
			if (other.realm != null)
				return false;
		} else if (!this.realm.equals(other.realm))
			return false;
		if (this.severity != other.severity)
			return false;
		return true;
	}
}
