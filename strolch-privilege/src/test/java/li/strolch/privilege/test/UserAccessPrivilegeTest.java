/*
 * Copyright (c) 2013-2025 Robert von Burg <eitch@eitchnet.ch>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 */
package li.strolch.privilege.test;

import li.strolch.privilege.base.AccessDeniedException;
import li.strolch.privilege.model.*;
import li.strolch.privilege.model.internal.User;
import li.strolch.privilege.model.internal.UserHistory;
import li.strolch.privilege.policy.UserAccessPrivilege;
import li.strolch.utils.collections.Tuple;
import org.junit.Test;

import java.time.ZonedDateTime;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import static li.strolch.privilege.handler.PrivilegeHandler.PRIVILEGE_GET_USER;
import static li.strolch.privilege.policy.PrivilegePolicyHelper.checkByAllowDenyValues;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class UserAccessPrivilegeTest {

	@Test
	public void shouldAllowEveryUserExceptDeniedUser() {
		Privilege privilege = privilege(Set.of("blocked"), Set.of());
		PrivilegeContext context = context();
		Restrictable restrictable = userRestrictable("allowed");

		assertTrue(new UserAccessPrivilege().hasPrivilege(context, privilege, restrictable));
		assertFalse(new UserAccessPrivilege().hasPrivilege(context, privilege, userRestrictable("blocked")));
	}

	@Test(expected = AccessDeniedException.class)
	public void shouldThrowWhenDeniedUserIsAsserted() {
		Privilege privilege = privilege(Set.of("blocked"), Set.of());
		new UserAccessPrivilege().validateAction(context(), privilege, userRestrictable("blocked"));
	}

	@Test
	public void shouldPreferDenyListOverAllowList() {
		Privilege privilege = privilege(Set.of("blocked"), Set.of("blocked", "allowed"));
		PrivilegeContext context = context();

		assertFalse(checkByAllowDenyValues(context, privilege, userRestrictable("blocked"), "blocked", false));
		assertTrue(checkByAllowDenyValues(context, privilege, userRestrictable("allowed"), "allowed", false));
	}

	@Test
	public void shouldRequireAllowListWhenNoDenyListExists() {
		Privilege privilege = privilege(Set.of(), Set.of("allowed"));
		PrivilegeContext context = context();

		assertTrue(checkByAllowDenyValues(context, privilege, userRestrictable("allowed"), "allowed", false));
		assertFalse(checkByAllowDenyValues(context, privilege, userRestrictable("unknown"), "unknown", false));
	}

	private static Privilege privilege(Set<String> denyList, Set<String> allowList) {
		return new Privilege(PRIVILEGE_GET_USER, UserAccessPrivilege.class.getSimpleName(), false, denyList, allowList);
	}

	private static Restrictable userRestrictable(String username) {
		User user = new User(null, username, null, "First", "Last", UserState.ENABLED, Set.of(), Set.of(),
				Locale.ENGLISH, Map.of(), false, UserHistory.EMPTY);
		return new Restrictable() {
			@Override
			public String getPrivilegeName() {
				return PRIVILEGE_GET_USER;
			}

			@Override
			public Object getPrivilegeValue() {
				return new Tuple(null, user);
			}
		};
	}

	private static PrivilegeContext context() {
		Certificate certificate = new Certificate(Usage.SINGLE, "session", "id", "admin", "First", "Last",
				UserState.ENABLED, "token", "test", ZonedDateTime.now(), false, Locale.ENGLISH, Set.of(), Set.of(),
				Map.of());
		return new PrivilegeContext(certificate, Map.of(PRIVILEGE_GET_USER, privilege(Set.of(), Set.of())), Map.of());
	}
}