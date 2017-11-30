package li.strolch.soql.core;

public class MockParameter {

	String value = "testValue";
	String type = "testType";

	public String getType() {
		return type;
	}

	@Override
	public String toString() {
		return "MockParameter [value=" + value + ", type=" + type + "]";
	}

}
