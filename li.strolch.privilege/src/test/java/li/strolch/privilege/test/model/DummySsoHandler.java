package li.strolch.privilege.test.model;

import java.util.*;
import java.util.stream.Collectors;

import li.strolch.privilege.base.PrivilegeException;
import li.strolch.privilege.handler.SingleSignOnHandler;
import li.strolch.privilege.model.UserState;
import li.strolch.privilege.model.internal.User;
import li.strolch.privilege.model.internal.UserHistory;

public class DummySsoHandler implements SingleSignOnHandler {

	private Map<String, String> parameterMap;

	@Override
	public Map<String, String> getParameterMap() {
		return this.parameterMap;
	}

	@Override
	public void initialize(Map<String, String> parameterMap) {
		this.parameterMap = parameterMap;
	}

	@Override
	public User authenticateSingleSignOn(Object data) throws PrivilegeException {

		@SuppressWarnings("unchecked")
		Map<String, String> map = (Map<String, String>) data;

		Set<String> roles = Arrays.stream(map.get("roles").split(",")).map(String::trim).collect(Collectors.toSet());
		Map<String, String> properties = new HashMap<>();
		return new User(map.get("userId"), map.get("username"), null, null, null, -1, -1, map.get("firstName"),
				map.get("lastName"), UserState.REMOTE, roles, Locale.ENGLISH, properties, new UserHistory());
	}
}
