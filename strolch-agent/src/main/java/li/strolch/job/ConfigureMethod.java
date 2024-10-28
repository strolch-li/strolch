package li.strolch.job;

public enum ConfigureMethod {
	Programmatic,
	Model;

	public boolean isProgrammatic() {
		return this == Programmatic;
	}

	public boolean isModel() {
		return this == Model;
	}
}
