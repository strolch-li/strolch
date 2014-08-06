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
package li.strolch.rest.inspector.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;

import li.strolch.runtime.query.enums.StrolchEnum;

import org.junit.Test;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class EnumTest extends AbstractRestfulTest {

	private static final String ROOT_PATH = "strolch/enums";

	@Test
	public void shouldQuerySex() {

		// query
		Response result = target().path(ROOT_PATH + "/sex").request(MediaType.APPLICATION_JSON).get();
		assertEquals(Status.OK.getStatusCode(), result.getStatus());
		StrolchEnum strolchEnum = result.readEntity(StrolchEnum.class);
		assertNotNull(strolchEnum);
		assertEquals("sex", strolchEnum.getName());
		assertEquals(4, strolchEnum.getValues().size());
	}

	@Test
	public void shouldQuerySalutation() {

		// query
		Response result = target().path(ROOT_PATH + "/salutation").request(MediaType.APPLICATION_JSON).get();
		assertEquals(Status.OK.getStatusCode(), result.getStatus());
		StrolchEnum strolchEnum = result.readEntity(StrolchEnum.class);
		assertNotNull(strolchEnum);
		assertEquals("salutation", strolchEnum.getName());
		assertEquals(3, strolchEnum.getValues().size());
	}
}
