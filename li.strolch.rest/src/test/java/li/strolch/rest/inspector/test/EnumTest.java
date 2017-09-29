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

import javax.ws.rs.core.HttpHeaders;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;
import java.util.Locale;

import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class EnumTest extends AbstractRestfulTest {

	private static final String ROOT_PATH = "strolch/enums";
	private String authToken;

	@Before
	public void before() {
		this.authToken = authenticate();
	}

	@After
	public void after() {
		if (this.authToken != null)
			logout(this.authToken);
	}

	@Test
	public void shouldQuerySex() {

		// query
		Response result = target() //
				.path(ROOT_PATH + "/sex") //
				.request(MediaType.APPLICATION_JSON) //
				.header(HttpHeaders.AUTHORIZATION, this.authToken) //
				.acceptLanguage(Locale.ENGLISH) //
				.get();
		assertEquals(Status.OK.getStatusCode(), result.getStatus());
		String strolchEnumS = result.readEntity(String.class);
		assertNotNull(strolchEnumS);
		JsonObject strolchEnumJ = new JsonParser().parse(strolchEnumS).getAsJsonObject();
		assertEquals("sex", strolchEnumJ.get("name").getAsString());
		assertEquals(4, strolchEnumJ.get("values").getAsJsonObject().size());
	}

	@Test
	public void shouldQuerySalutation() {

		// query
		Response result = target() //
				.path(ROOT_PATH + "/salutation") //
				.request(MediaType.APPLICATION_JSON) //
				.header(HttpHeaders.AUTHORIZATION, this.authToken) //
				.acceptLanguage(Locale.ENGLISH) //
				.get();
		assertEquals(Status.OK.getStatusCode(), result.getStatus());
		String strolchEnumS = result.readEntity(String.class);
		assertNotNull(strolchEnumS);
		JsonObject strolchEnumJ = new JsonParser().parse(strolchEnumS).getAsJsonObject();
		assertEquals("salutation", strolchEnumJ.get("name").getAsString());
		assertEquals(3, strolchEnumJ.get("values").getAsJsonObject().size());
		assertEquals("Mrs", strolchEnumJ.get("values").getAsJsonObject().get("mrs").getAsString());
	}

	@Test
	public void shouldQueryGermanSalutation() {

		// query
		Response result = target() //
				.path(ROOT_PATH + "/salutation") //
				.request(MediaType.APPLICATION_JSON) //
				.header(HttpHeaders.AUTHORIZATION, this.authToken) //
				.acceptLanguage(Locale.GERMAN) //
				.get();
		assertEquals(Status.OK.getStatusCode(), result.getStatus());
		String strolchEnumS = result.readEntity(String.class);
		assertNotNull(strolchEnumS);
		JsonObject strolchEnumJ = new JsonParser().parse(strolchEnumS).getAsJsonObject();
		assertEquals("salutation", strolchEnumJ.get("name").getAsString());
		assertEquals(3, strolchEnumJ.get("values").getAsJsonObject().size());
		assertEquals("Frau", strolchEnumJ.get("values").getAsJsonObject().get("mrs").getAsString());
	}
}
