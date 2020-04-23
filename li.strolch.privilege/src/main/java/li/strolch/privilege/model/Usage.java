package li.strolch.privilege.model;

import li.strolch.privilege.base.PrivilegeException;

public enum Usage {
	ANY("any"),
	SINGLE("single"),
	SET_PASSWORD("set-password");

	private final String value;

	Usage(String value) {
		this.value = value;
	}

	public String getValue() {
		return this.value;
	}

	public boolean isAny() {
		return this == ANY;
	}

	public boolean isSingle() {
		return this == SINGLE;
	}

	public boolean isSetPassword() {
		return this == SET_PASSWORD;
	}

	public static Usage byValue(String value) {
		for (Usage usage : values()) {
			if (usage.value.equals(value))
				return usage;
		}

		throw new PrivilegeException("No Usage found with value: " + value);
	}
}
