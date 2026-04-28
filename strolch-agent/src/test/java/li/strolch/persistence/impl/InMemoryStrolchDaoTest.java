/*
 * Copyright (c) 2013-2025 Robert von Burg <eitch@eitchnet.ch>
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

package li.strolch.persistence.impl;

import li.strolch.model.Resource;
import org.junit.Test;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class InMemoryStrolchDaoTest {

	@Test
	public void shouldHaveElementByElement() {
		InMemoryStrolchDao<Resource> dao = new InMemoryStrolchDao<>();
		Resource existing = new Resource("resource1", "Resource 1", "MyType");
		Resource missing = new Resource("resource2", "Resource 2", "MyType");

		dao.save(existing);

		assertTrue(dao.hasElement(existing));
		assertFalse(dao.hasElement(missing));
	}

	@Test
	public void shouldHaveElementByTypeAndId() {
		InMemoryStrolchDao<Resource> dao = new InMemoryStrolchDao<>();
		Resource existing = new Resource("resource1", "Resource 1", "MyType");

		dao.save(existing);

		assertTrue(dao.hasElement("MyType", "resource1"));
		assertFalse(dao.hasElement("MyType", "resource2"));
		assertFalse(dao.hasElement("OtherType", "resource1"));
	}
}
