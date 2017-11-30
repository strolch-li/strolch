package li.strolch.soql.core;

import java.util.HashMap;
import java.util.Map;

public class MockObject {

	String id = "testId";
	String name = "testName";
	String type = "testType";
	
	Map<String, MockParameter> parameter = new HashMap<>(); 

	int number = 42;

	public String getType() {
		return type;
	}

	public String getName() {
		return name;
	}

	public String getId() {
		return id;
	}

	public int getNumber() {
		return number;
	}

	public Object getParameter(String key) {
		return parameter.get(key);
	}
	
	public Object getParameter(String key, String dummy) {
		return parameter.get(key);
	}
	
	public void putParameter(String key, MockParameter value) {
		this.parameter.put(key, value);
	}
	
	@Override
	public String toString() {
		return "MockObject [id=" + id + ", name=" + name + ", type=" + type + ", number=" + number + "]";
	}

}
