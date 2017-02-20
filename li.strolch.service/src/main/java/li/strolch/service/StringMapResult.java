package li.strolch.service;

import java.util.HashMap;
import java.util.Map;

import li.strolch.service.api.ServiceResult;
import li.strolch.service.api.ServiceResultState;

public class StringMapResult extends ServiceResult {

	private static final long serialVersionUID = 1L;

	private Map<String, String> map = new HashMap<>();

	public StringMapResult(ServiceResultState state) {
		super(state);
	}

	public StringMapResult(ServiceResultState state, String msg) {
		super(state, msg);
	}

	public StringMapResult(String key, String value) {
		super(ServiceResultState.SUCCESS);
		this.map = new HashMap<>();
		this.map.put(key, value);
	}

	public StringMapResult(String key1, String value1, String key2, String value2) {
		super(ServiceResultState.SUCCESS);
		this.map = new HashMap<>();
		this.map.put(key1, value1);
		this.map.put(key2, value2);
	}

	public Map<String, String> getMap() {
		return this.map;
	}

	public void put(String key, String value) {
		this.map.put(key, value);
	}
}
