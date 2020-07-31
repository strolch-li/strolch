package li.strolch.model.log;

import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Objects;
import java.util.Properties;
import java.util.ResourceBundle;

import com.google.gson.JsonObject;
import li.strolch.model.Locator;
import li.strolch.model.Tags.Json;
import li.strolch.utils.I18nMessage;
import li.strolch.utils.helper.ExceptionHelper;
import li.strolch.utils.helper.StringHelper;
import li.strolch.utils.iso8601.ISO8601;

public class LogMessage extends I18nMessage {

	private final String id;
	private final String username;
	private final ZonedDateTime zonedDateTime;
	private final String realm;
	private final Locator locator;
	private final LogSeverity severity;
	private LogMessageState state;
	private String stackTrace;

	public LogMessage(String realm, String username, Locator locator, LogSeverity severity, LogMessageState state,
			I18nMessage i18nMessage) {
		super(i18nMessage);
		this.id = StringHelper.getUniqueId();
		// persisting in the DB only handles millisecond precision, not nano precision
		ZonedDateTime now = ZonedDateTime.now();
		this.zonedDateTime = now.withNano((now.getNano() / 1000000) * 1000000);
		this.realm = realm;
		this.username = username;
		this.locator = locator;
		this.severity = severity;
		this.state = state;
	}

	public LogMessage(String realm, String username, Locator locator, LogSeverity severity, LogMessageState state,
			ResourceBundle bundle, String key) {
		super(bundle, key);
		this.id = StringHelper.getUniqueId();
		// persisting in the DB only handles millisecond precision, not nano precision
		ZonedDateTime now = ZonedDateTime.now();
		this.zonedDateTime = now.withNano((now.getNano() / 1000000) * 1000000);
		this.realm = realm;
		this.username = username;
		this.locator = locator;
		this.severity = severity;
		this.state = state;
	}

	public LogMessage(String id, ZonedDateTime zonedDateTime, String realm, String username, Locator locator,
			LogSeverity severity, LogMessageState state, String key, Properties values, String message,
			String stackTrace) {
		super(key, values, message);
		this.id = id;
		this.zonedDateTime = zonedDateTime;
		this.realm = realm;
		this.username = username;
		this.locator = locator;
		this.severity = severity;
		this.state = state;
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

	public LogMessageState getState() {
		return this.state;
	}

	public void setState(LogMessageState state) {
		this.state = state;
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
		jsonObject.addProperty(Json.STATE, this.state.name());
		jsonObject.addProperty(Json.USERNAME, this.username);
		jsonObject.addProperty(Json.REALM, this.realm);
		jsonObject.addProperty(Json.LOCATOR, this.locator.toString());
		if (this.stackTrace != null)
			jsonObject.addProperty(Json.EXCEPTION, this.stackTrace);
		JsonObject values = new JsonObject();
		for (String key : getValues().stringPropertyNames()) {
			values.addProperty(key, getValues().getProperty(key));
		}
		jsonObject.add(Json.VALUES, values);

		return jsonObject;
	}

	public static LogMessage fromJson(JsonObject messageJ) {

		String id = messageJ.get(Json.ID).getAsString();
		ZonedDateTime zonedDateTime = ISO8601.parseToZdt(messageJ.get(Json.DATE).getAsString());
		String realm = messageJ.get(Json.REALM).getAsString();
		String username = messageJ.get(Json.USERNAME).getAsString();
		Locator locator = Locator.valueOf(messageJ.get(Json.LOCATOR).getAsString());
		LogSeverity severity = LogSeverity.valueOf(messageJ.get(Json.SEVERITY).getAsString());
		LogMessageState state = LogMessageState.valueOf(messageJ.get(Json.STATE).getAsString());
		String key = messageJ.get(Json.KEY).getAsString();
		String message = messageJ.get(Json.MESSAGE).getAsString();
		String stackTrace = messageJ.has(Json.EXCEPTION) ? messageJ.get(Json.EXCEPTION).getAsString() : "";

		Properties properties = new Properties();
		if (messageJ.has(Json.VALUES)) {
			JsonObject valuesJ = messageJ.getAsJsonObject(Json.VALUES);
			for (String propertyName : valuesJ.keySet()) {
				properties.setProperty(propertyName, valuesJ.get(propertyName).getAsString());
			}
		}

		return new LogMessage(id, zonedDateTime, realm, username, locator, severity, state, key, properties, message,
				stackTrace);
	}

	@Override
	public boolean equals(Object o) {
		if (this == o)
			return true;
		if (o == null || getClass() != o.getClass())
			return false;
		if (!super.equals(o))
			return false;

		LogMessage that = (LogMessage) o;
		return Objects.equals(id, that.id);
	}

	@Override
	public int hashCode() {
		int result = super.hashCode();
		result = 31 * result + (id != null ? id.hashCode() : 0);
		return result;
	}
}
