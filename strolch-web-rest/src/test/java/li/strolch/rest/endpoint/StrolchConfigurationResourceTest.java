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
package li.strolch.rest.endpoint;

import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import jakarta.ws.rs.client.Entity;
import jakarta.ws.rs.core.MediaType;
import jakarta.ws.rs.core.Response;
import li.strolch.rest.inspector.test.AbstractRestfulTest;
import li.strolch.testbase.runtime.RuntimeMock;
import org.glassfish.jersey.server.ResourceConfig;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

import java.io.File;

import static li.strolch.rest.StrolchRestfulConstants.DATA;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

public class StrolchConfigurationResourceTest extends AbstractRestfulTest {

	private static final String RUNTIME_PATH = "target/StrolchConfigurationResourceTest/";
	private static final String CONFIG_SRC = "src/test/resources/configtest";

	@BeforeClass
	public static void beforeClass() throws IllegalArgumentException {
		File rootPath = new File(RUNTIME_PATH);
		File configSrc = new File(CONFIG_SRC);
		RuntimeMock runtimeMock = new RuntimeMock();
		runtimeMock.mockRuntime(rootPath, configSrc);
		runtimeMock.startContainer();
	}

	@AfterClass
	public static void afterClass() {
		RuntimeMock runtimeMock = new RuntimeMock();
		runtimeMock.mockRuntime(new File(RUNTIME_PATH), new File(CONFIG_SRC));
		runtimeMock.destroyRuntime();
	}

	@Override
	protected ResourceConfig configure() {
		ResourceConfig resourceConfig = super.configure();
		resourceConfig.property(li.strolch.rest.RestfulStrolchComponent.class.getName(),
				li.strolch.rest.RestfulStrolchComponent.getInstance());
		return resourceConfig;
	}

	@Test
	public void shouldGetConfigurationResource() {
		String authToken = authenticate("admin", "admin");
		try (Response response = target()
				.path("strolch/configuration/resource")
				.request(MediaType.APPLICATION_JSON)
				.header("Authorization", authToken)
				.get()) {
			assertEquals(Response.Status.OK.getStatusCode(), response.getStatus());
			JsonObject result = JsonParser.parseString(response.readEntity(String.class)).getAsJsonObject();
			assertNotNull(result.get(DATA));
		} finally {
			logout("admin", authToken);
		}
	}

	@Test
	public void shouldUpdateConfigurationResource() {
		String authToken = authenticate("admin", "admin");
		try {
			JsonObject resource;
			try (Response response = target()
					.path("strolch/configuration/resource")
					.request(MediaType.APPLICATION_JSON)
					.header("Authorization", authToken)
					.get()) {
				assertEquals(Response.Status.OK.getStatusCode(), response.getStatus());
				JsonObject result = JsonParser.parseString(response.readEntity(String.class)).getAsJsonObject();
				resource = result.getAsJsonObject(DATA);
			}

			// update name
			resource.addProperty("name", "Updated Configuration");

			try (Response response = target()
					.path("strolch/configuration/resource")
					.request(MediaType.APPLICATION_JSON)
					.header("Authorization", authToken)
					.put(Entity.json(resource.toString()))) {
				assertEquals(Response.Status.OK.getStatusCode(), response.getStatus());
			}

			try (Response response = target()
					.path("strolch/configuration/resource")
					.request(MediaType.APPLICATION_JSON)
					.header("Authorization", authToken)
					.get()) {
				assertEquals(Response.Status.OK.getStatusCode(), response.getStatus());
				JsonObject result = JsonParser.parseString(response.readEntity(String.class)).getAsJsonObject();
				assertEquals("Updated Configuration", result.getAsJsonObject(DATA).get("name").getAsString());
			}
		} finally {
			logout("admin", authToken);
		}
	}

	@Test
	public void shouldGetPolicies() {
		String authToken = authenticate("admin", "admin");
		try (Response response = target()
				.path("strolch/configuration/policies")
				.request(MediaType.APPLICATION_JSON)
				.header("Authorization", authToken)
				.get()) {
			assertEquals(Response.Status.OK.getStatusCode(), response.getStatus());
			JsonObject result = JsonParser.parseString(response.readEntity(String.class)).getAsJsonObject();
			JsonObject data = result.getAsJsonObject(DATA);
			assertNotNull(data.get("policyTypes"));
		} finally {
			logout("admin", authToken);
		}
	}

	@Test
	public void shouldUpdatePolicies() {
		String authToken = authenticate("admin", "admin");
		try {
			JsonObject policyModel;
			try (Response response = target()
					.path("strolch/configuration/policies")
					.request(MediaType.APPLICATION_JSON)
					.header("Authorization", authToken)
					.get()) {
				assertEquals(Response.Status.OK.getStatusCode(), response.getStatus());
				JsonObject result = JsonParser.parseString(response.readEntity(String.class)).getAsJsonObject();
				policyModel = result.getAsJsonObject(DATA);
			}

			try (Response response = target()
					.path("strolch/configuration/policies")
					.request(MediaType.APPLICATION_JSON)
					.header("Authorization", authToken)
					.put(Entity.json(policyModel.toString()))) {
				assertEquals(Response.Status.OK.getStatusCode(), response.getStatus());
			}
		} finally {
			logout("admin", authToken);
		}
	}
}
