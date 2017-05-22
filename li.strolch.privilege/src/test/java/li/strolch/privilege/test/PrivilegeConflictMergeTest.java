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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import li.strolch.privilege.model.IPrivilege;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class PrivilegeConflictMergeTest extends AbstractPrivilegeTest {

	@BeforeClass
	public static void init() throws Exception {
		removeConfigs(PrivilegeConflictMergeTest.class.getSimpleName());
		prepareConfigs(PrivilegeConflictMergeTest.class.getSimpleName(), "PrivilegeConfigMerge.xml",
				"PrivilegeUsersMerge.xml", "PrivilegeRolesMerge.xml");
	}

	@AfterClass
	public static void destroy() throws Exception {
		removeConfigs(PrivilegeConflictMergeTest.class.getSimpleName());
	}

	@Before
	public void setup() throws Exception {
		initialize(PrivilegeConflictMergeTest.class.getSimpleName(), "PrivilegeConfigMerge.xml");
	}

	@Test
	public void shouldMergePrivileges1() {
		try {
			login("userA", "admin".toCharArray());
			IPrivilege privilege = this.ctx.getPrivilege("Foo");
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
			IPrivilege privilege = this.ctx.getPrivilege("Bar");
			assertFalse(privilege.isAllAllowed());
			assertEquals(2, privilege.getAllowList().size());
			assertEquals(2, privilege.getDenyList().size());
		} finally {
			logout();
		}
	}
}
