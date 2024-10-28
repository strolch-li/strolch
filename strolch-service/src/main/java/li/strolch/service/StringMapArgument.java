package li.strolch.service;

import li.strolch.service.api.ServiceArgument;

import java.util.HashMap;
import java.util.Map;

public class StringMapArgument extends ServiceArgument {
	public final Map<String, String> map = new HashMap<>();
}
