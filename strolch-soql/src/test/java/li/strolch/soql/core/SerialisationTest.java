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

package li.strolch.soql.core;

import com.google.gson.JsonObject;
import org.junit.Test;

import java.util.List;

import static org.junit.Assert.assertEquals;

/**
 * @author msmock
 */
public class SerialisationTest extends BaseTest {

	private QueryRequest buildTestRequest() {
		String s = "SELECT a FROM Activity a WHERE a.getId() = :p";
		final QueryRequest request = new QueryRequest();
		request.setStatement(s);
		request.addParameter("r", "Just a string!");
		return request;
	}

	private QueryResponse buildTestResponse() {
		String s = "SELECT a FROM Activity a WHERE a.getId() = :p";
		final QueryResponse response = new QueryResponse();
		response.setStatement(s);
		response.addParameter("r", "Just a string!");
		return response;
	}

	@Test
	public void testQuery2JSON() {
		final QueryRequest request = buildTestRequest();
		JsonObject jsonObject = request.asJson();

		String expected = "{\"objectType\":\"QueryRequest\",\"statement\":\"SELECT a FROM Activity a WHERE a.getId() "
				+ "= :p\",\"queryParameter\":{\"r\":\"Just a string!\"}}";
		assertEquals(expected.trim(), jsonObject.toString());
	}

	@Test
	public void testJSON2Query() {

		String s = "SELECT a FROM Activity a WHERE a.getId() = :p";

		final QueryRequest initial = new QueryRequest();
		initial.setStatement(s);
		initial.addParameter("p", "10010");
		final JsonObject jsonObject = initial.asJson();

		final QueryRequest query = QueryRequest.fromJson(jsonObject);

		assertEquals(s, query.getStatement());
		assertEquals("10010", query.getParameterMap().get("p"));
	}

	@SuppressWarnings({"unchecked", "rawtypes"})
	@Test
	public void testResponse2JSON() {
		final QueryResponse response = buildTestResponse();
		final List evalResult = getTestRessources(2);
		response.getResultSet().add(evalResult);

		String expected
				= "{\"objectType\":\"QueryResponse\",\"statement\":\"SELECT a FROM Activity a WHERE a.getId() = :p\",\"queryParameter\":{\"r\":\"Just a string!\"},\"resultSet\":[[{\"objectType\":\"Resource\",\"id\":\"0\",\"name\":null,\"type\":null,\"parameterBags\":{\"testBag\":{\"id\":\"testBag\",\"name\":null,\"type\":null,\"parameters\":{\"testId\":{\"id\":\"testId\",\"name\":null,\"type\":\"Float\",\"value\":100.0}}}}},{\"objectType\":\"Resource\",\"id\":\"1\",\"name\":null,\"type\":null,\"parameterBags\":{\"testBag\":{\"id\":\"testBag\",\"name\":null,\"type\":null,\"parameters\":{\"testId\":{\"id\":\"testId\",\"name\":null,\"type\":\"Float\",\"value\":100.0}}}}}]]}";

		final JsonObject jsonObject = response.asJson();
		assertEquals(expected.trim(), jsonObject.toString());

		System.out.println(jsonObject);
	}

}
