package li.strolch.handler.operationslog;

public enum LogSeverity {
	INFO("Info"), //
	NOTIFICATION("Notification"), //
	WARN("Warn"), //
	ERROR("Error"), //
	EXCEPTION("Exception");

	private String severity;

	private LogSeverity(String severity) {
		this.severity = severity;
	}

	public String getSeverity() {
		return this.severity;
	}

	public static LogSeverity from(String value) {
		for (LogSeverity type : values()) {
			if (type.severity.equals(value))
				return type;
		}

		throw new IllegalArgumentException("No severity exists for " + value);
	}
}
