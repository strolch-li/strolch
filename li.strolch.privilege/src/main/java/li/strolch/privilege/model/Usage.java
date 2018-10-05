package li.strolch.privilege.model;

import li.strolch.privilege.base.PrivilegeException;

public enum Usage {
	ANY("any"),
	SET_PASSWORD("set-password");

	private String value;

	private Usage(String value) {
		this.value = value;
	}

	public String getValue() {
		return this.value;
	}

	public static Usage byValue(String value) {
		for (Usage usage : values()) {
			if (usage.value.equals(value))
				return usage;
		}

		throw new PrivilegeException("No Usage found with value: " + value);
	}
}
