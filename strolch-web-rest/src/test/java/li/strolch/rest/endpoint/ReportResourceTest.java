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

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import jakarta.ws.rs.client.Entity;
import jakarta.ws.rs.core.MediaType;
import jakarta.ws.rs.core.Response;
import li.strolch.model.Tags;
import li.strolch.report.ReportConstants;
import li.strolch.rest.inspector.test.AbstractRestfulTest;
import li.strolch.testbase.runtime.RuntimeMock;
import org.glassfish.jersey.server.ResourceConfig;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;

import java.io.File;

import static li.strolch.rest.StrolchRestfulConstants.*;
import static org.junit.Assert.*;

@RunWith(JUnit4.class)
public class ReportResourceTest extends AbstractRestfulTest {

	private static final String RUNTIME_PATH = "target/ReportResourceTest/";
	private static final String CONFIG_SRC = "src/test/resources/reporttest";

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
	public void shouldGetAllReportIds() {
		String authToken = authenticate("test", "test");
		try (Response response = target()
				.path("strolch/reports")
				.request(MediaType.APPLICATION_JSON)
				.header("Authorization", authToken)
				.get()) {
			assertEquals(Response.Status.OK.getStatusCode(), response.getStatus());
			JsonObject result = JsonParser.parseString(response.readEntity(String.class)).getAsJsonObject();
			JsonArray data = result.getAsJsonArray(Tags.Json.DATA);
			assertFalse(data.isEmpty());
		} finally {
			logout("test", authToken);
		}
	}

	@Test
	public void shouldGetReportFacets() {
		String authToken = authenticate("test", "test");
		try (Response response = target()
				.path("strolch/reports/stockReport/facets")
				.request(MediaType.APPLICATION_JSON)
				.header("Authorization", authToken)
				.get()) {
			assertEquals(Response.Status.OK.getStatusCode(), response.getStatus());
			JsonObject result = JsonParser.parseString(response.readEntity(String.class)).getAsJsonObject();
			JsonObject data = result.getAsJsonObject(Tags.Json.DATA);
			JsonArray facets = data.getAsJsonArray(PARAM_FACETS);
			assertFalse(facets.isEmpty());
		} finally {
			logout("test", authToken);
		}
	}

	@Test
	public void shouldGetReportById() {
		String authToken = authenticate("test", "test");

		JsonObject query = new JsonObject();
		query.addProperty(OFFSET, 0);
		query.addProperty(LIMIT, 10);

		try (Response response = target()
				.path("strolch/reports/stockReport")
				.request(MediaType.APPLICATION_JSON)
				.header("Authorization", authToken)
				.post(Entity.json(query.toString()))) {
			assertEquals(Response.Status.OK.getStatusCode(), response.getStatus());
			JsonObject result = JsonParser.parseString(response.readEntity(String.class)).getAsJsonObject();
			JsonObject data = result.getAsJsonObject(Tags.Json.DATA);
			JsonArray rows = data.getAsJsonArray(ReportConstants.PARAM_ROWS);
			assertEquals(4, rows.size());
		} finally {
			logout("test", authToken);
		}
	}

	@Test
	public void shouldGetReportWithFilters() {
		String authToken = authenticate("test", "test");

		JsonObject query = new JsonObject();
		query.addProperty(OFFSET, 0);
		query.addProperty(LIMIT, 10);

		JsonArray filters = new JsonArray();
		JsonObject filter = new JsonObject();
		filter.addProperty(ReportConstants.PARAM_FACET_TYPE, "Product");
		JsonArray facetFilters = new JsonArray();
		facetFilters.add("product01");
		filter.add(ReportConstants.PARAM_FACET_FILTERS, facetFilters);
		filters.add(filter);
		query.add(PARAM_FILTER, filters);

		try (Response response = target()
				.path("strolch/reports/stockReport")
				.request(MediaType.APPLICATION_JSON)
				.header("Authorization", authToken)
				.post(Entity.json(query.toString()))) {
			assertEquals(Response.Status.OK.getStatusCode(), response.getStatus());
			JsonObject result = JsonParser.parseString(response.readEntity(String.class)).getAsJsonObject();
			JsonObject data = result.getAsJsonObject(Tags.Json.DATA);
			JsonArray rows = data.getAsJsonArray(ReportConstants.PARAM_ROWS);
			assertEquals(2, rows.size());

			for (JsonElement row : rows) {
				assertEquals("Product 01", row.getAsJsonObject().get("product").getAsString());
			}
		} finally {
			logout("test", authToken);
		}
	}

	@Test
	public void shouldGetReportWithDateRange() {
		String authToken = authenticate("test", "test");

		JsonObject query = new JsonObject();
		query.addProperty(OFFSET, 0);
		query.addProperty(LIMIT, 10);

		JsonObject dateRange = new JsonObject();
		dateRange.addProperty(PARAM_FROM, "2016-01-01T00:00:00.000Z");
		dateRange.addProperty(PARAM_TO, "2017-01-01T00:00:00.000Z");
		query.add(PARAM_DATE_RANGE, dateRange);

		try (Response response = target()
				.path("strolch/reports/stockReport")
				.request(MediaType.APPLICATION_JSON)
				.header("Authorization", authToken)
				.post(Entity.json(query.toString()))) {
			assertEquals(Response.Status.OK.getStatusCode(), response.getStatus());
			JsonObject result = JsonParser.parseString(response.readEntity(String.class)).getAsJsonObject();
			JsonObject data = result.getAsJsonObject(Tags.Json.DATA);
			JsonArray rows = data.getAsJsonArray(ReportConstants.PARAM_ROWS);
			assertEquals(0, rows.size());
		}

		// expanded range
		dateRange.addProperty(PARAM_TO, "2017-03-01T00:00:00.000Z");
		try (Response response = target()
				.path("strolch/reports/stockReport")
				.request(MediaType.APPLICATION_JSON)
				.header("Authorization", authToken)
				.post(Entity.json(query.toString()))) {
			assertEquals(Response.Status.OK.getStatusCode(), response.getStatus());
			JsonObject result = JsonParser.parseString(response.readEntity(String.class)).getAsJsonObject();
			JsonObject data = result.getAsJsonObject(Tags.Json.DATA);
			JsonArray rows = data.getAsJsonArray(ReportConstants.PARAM_ROWS);
			assertEquals(4, rows.size());
		} finally {
			logout("test", authToken);
		}
	}

	@Test
	public void shouldGetReportByIdAsCsv() {
		String authToken = authenticate("test", "test");

		JsonObject query = new JsonObject();
		JsonArray filters = new JsonArray();
		JsonObject filter = new JsonObject();
		filter.addProperty(ReportConstants.PARAM_FACET_TYPE, "Product");
		JsonArray facetFilters = new JsonArray();
		facetFilters.add("product01");
		filter.add(ReportConstants.PARAM_FACET_FILTERS, facetFilters);
		filters.add(filter);
		query.add(PARAM_FILTER, filters);

		try (Response response = target()
				.path("strolch/reports/stockReport/csv")
				.request(TEXT_CSV)
				.header("Authorization", authToken)
				.post(Entity.json(query.toString()))) {
			assertEquals(Response.Status.OK.getStatusCode(), response.getStatus());
			System.out.println("[DEBUG_LOG] Media Type: " + response.getMediaType());
			assertTrue("Media type should start with text/csv", response.getMediaType().toString().startsWith(TEXT_CSV));
			byte[] responseBytes = response.readEntity(byte[].class);
			assertNotNull(responseBytes);
			assertTrue("Response should not be empty", responseBytes.length > 3);

			// Check for UTF-8 BOM: 0xEF, 0xBB, 0xBF
			assertEquals((byte) 0xEF, responseBytes[0]);
			assertEquals((byte) 0xBB, responseBytes[1]);
			assertEquals((byte) 0xBF, responseBytes[2]);

			String csv = new String(responseBytes, 3, responseBytes.length - 3, java.nio.charset.StandardCharsets.UTF_8);
			System.out.println("[DEBUG_LOG] CSV Content:\n" + csv);
			assertFalse(csv.isEmpty());

			String[] lines = csv.split("\n");
			assertTrue("CSV should have at least 2 lines (header + 1 data line), but was: " + lines.length,
					lines.length >= 2);

			// Check header
			assertTrue(lines[0].contains("product"));
			assertTrue(lines[0].contains("quantity"));

			// Check data (product01 matches 2 rows in original test, but with filter it should be there)
			assertTrue(csv.contains("Product 01"));
		} finally {
			logout("test", authToken);
		}
	}
}
