package li.strolch.service;

import java.util.HashMap;
import java.util.Map;

import li.strolch.service.api.ServiceArgument;

public class StringMapArgument extends ServiceArgument {
	public Map<String, String> map = new HashMap<>();
}
