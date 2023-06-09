package li.strolch.runtime.configuration;

public class SupportedLanguage {
	private final String locale;
	private final String name;

	public SupportedLanguage(String locale, String name) {
		this.locale = locale;
		this.name = name;
	}

	public String getLocale() {
		return locale;
	}

	public String getName() {
		return name;
	}
}
