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

import javax.ws.rs.core.HttpHeaders;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@SuppressWarnings("nls")
public class VersionQueryTest extends AbstractRestfulTest {

	private static final String ROOT_PATH = "strolch/version";
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
	public void shouldQueryVersion() {

		// query
		Response result = target() //
				.path(ROOT_PATH) //
				.request(MediaType.APPLICATION_JSON) //
				.header(HttpHeaders.AUTHORIZATION, this.authToken) //
				.get();
		assertEquals(Status.OK.getStatusCode(), result.getStatus());
		String versionQueryResultS = result.readEntity(String.class);

		JsonObject versionQueryResultJ = JsonParser.parseString(versionQueryResultS).getAsJsonObject();

		if (versionQueryResultJ.has("errors")) {
			JsonArray errorsJ = versionQueryResultJ.get("errors").getAsJsonArray();
			for (JsonElement errorJ : errorsJ) {
				logger.error(errorJ.getAsString());
			}
		}

		JsonObject agentVersionJ = versionQueryResultJ.get("agentVersion").getAsJsonObject();
		assertEquals("StrolchPersistenceTest", agentVersionJ.get("agentName").getAsString());
		assertEquals("li.strolch", agentVersionJ.get("groupId").getAsString());
		assertEquals("li.strolch.agent", agentVersionJ.get("artifactId").getAsString());

		JsonArray componentVersionsJ = versionQueryResultJ.get("componentVersions").getAsJsonArray();
		assertEquals(6, componentVersionsJ.size());
		for (JsonElement element : componentVersionsJ) {
			JsonObject componentVersionJ = element.getAsJsonObject();
			assertEquals("li.strolch", componentVersionJ.get("groupId").getAsString());
		}
	}
}
