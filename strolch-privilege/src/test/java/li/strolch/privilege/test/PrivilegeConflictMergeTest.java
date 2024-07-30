/*
 * Copyright 2013 Robert von Burg <eitch@eitchnet.ch>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package li.strolch.privilege.test;

import li.strolch.privilege.model.Privilege;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import java.util.Set;

import static org.junit.Assert.*;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class PrivilegeConflictMergeTest extends AbstractPrivilegeTest {

	@BeforeClass
	public static void init() {
		removeConfigs(PrivilegeConflictMergeTest.class.getSimpleName());
		prepareConfigs(PrivilegeConflictMergeTest.class.getSimpleName(), "PrivilegeConfigMerge.xml",
				"PrivilegeUsersMerge.xml", "PrivilegeGroupsMerge.xml", "PrivilegeRolesMerge.xml");
	}

	@AfterClass
	public static void destroy() {
		removeConfigs(PrivilegeConflictMergeTest.class.getSimpleName());
	}

	@Before
	public void setup() {
		initialize(PrivilegeConflictMergeTest.class.getSimpleName(), "PrivilegeConfigMerge.xml");
	}

	@Test
	public void shouldMergePrivileges1() {
		try {
			login("userA", "admin".toCharArray());
			assertEquals(Set.of(), this.ctx.getUserRep().getGroups());
			assertFalse(this.ctx.hasGroup("GroupA1"));
			assertFalse(this.ctx.hasGroup("GroupA2"));
			assertEquals(Set.of("RoleA1", "RoleA2"), this.ctx.getUserRep().getRoles());
			assertTrue(this.ctx.hasRole("RoleA1"));
			assertTrue(this.ctx.hasRole("RoleA2"));
			assertFalse(this.ctx.hasRole("RoleB2"));
			assertNull(this.ctx.getUserRep().getLocation());
			assertEquals(Set.of(), this.ctx.getUserRep().getPropertyKeySet());
			Privilege privilege = this.ctx.getPrivilege("Foo");
			assertTrue(privilege.isAllAllowed());
			assertTrue(privilege.getAllowList().isEmpty());
			assertTrue(privilege.getDenyList().isEmpty());

		} finally {
			logout();
		}
	}

	@Test
	public void shouldMergePrivileges2() {
		try {
			login("userB", "admin".toCharArray());
			assertEquals(Set.of(), this.ctx.getUserRep().getGroups());
			assertFalse(this.ctx.hasGroup("GroupB1"));
			assertFalse(this.ctx.hasGroup("GroupB2"));
			assertEquals(Set.of("RoleB1", "RoleB2"), this.ctx.getUserRep().getRoles());
			assertTrue(this.ctx.hasRole("RoleB1"));
			assertTrue(this.ctx.hasRole("RoleB2"));
			assertFalse(this.ctx.hasRole("RoleA2"));
			assertNull(this.ctx.getUserRep().getLocation());
			assertEquals(Set.of(), this.ctx.getUserRep().getPropertyKeySet());
			Privilege privilege = this.ctx.getPrivilege("Bar");
			assertFalse(privilege.isAllAllowed());
			assertEquals(2, privilege.getAllowList().size());
			assertEquals(2, privilege.getDenyList().size());
		} finally {
			logout();
		}
	}

	@Test
	public void shouldMergePrivileges3() {
		try {
			login("userC", "admin".toCharArray());
			assertEquals(Set.of("GroupA1", "GroupA2"), this.ctx.getUserRep().getGroups());
			assertTrue(this.ctx.hasGroup("GroupA1"));
			assertTrue(this.ctx.hasGroup("GroupA2"));
			assertFalse(this.ctx.hasGroup("GroupB2"));
			assertEquals(Set.of("RoleA1", "RoleA2"), this.ctx.getUserRep().getRoles());
			assertTrue(this.ctx.hasRole("RoleA1"));
			assertTrue(this.ctx.hasRole("RoleA2"));
			assertFalse(this.ctx.hasRole("RoleB2"));
			assertEquals("LocationA2", this.ctx.getUserRep().getLocation());
			assertEquals(Set.of("location"), this.ctx.getUserRep().getPropertyKeySet());
			Privilege privilege = this.ctx.getPrivilege("Foo");
			assertTrue(privilege.isAllAllowed());
			assertTrue(privilege.getAllowList().isEmpty());
			assertTrue(privilege.getDenyList().isEmpty());

		} finally {
			logout();
		}
	}

	@Test
	public void shouldMergePrivileges4() {
		try {
			login("userD", "admin".toCharArray());
			assertEquals(Set.of("GroupB1", "GroupB2"), this.ctx.getUserRep().getGroups());
			assertTrue(this.ctx.hasGroup("GroupB1"));
			assertTrue(this.ctx.hasGroup("GroupB2"));
			assertFalse(this.ctx.hasGroup("GroupA2"));
			assertEquals(Set.of("RoleB1", "RoleB2"), this.ctx.getUserRep().getRoles());
			assertTrue(this.ctx.hasRole("RoleB1"));
			assertTrue(this.ctx.hasRole("RoleB2"));
			assertFalse(this.ctx.hasRole("RoleA2"));
			assertEquals("LocationB2", this.ctx.getUserRep().getLocation());
			assertEquals(Set.of("location"), this.ctx.getUserRep().getPropertyKeySet());
			Privilege privilege = this.ctx.getPrivilege("Bar");
			assertFalse(privilege.isAllAllowed());
			assertEquals(2, privilege.getAllowList().size());
			assertEquals(2, privilege.getDenyList().size());
			assertEquals(Set.of("allow1", "allow2"), privilege.getAllowList());
			assertEquals(Set.of("deny1", "deny2"), privilege.getDenyList());
		} finally {
			logout();
		}
	}
}
