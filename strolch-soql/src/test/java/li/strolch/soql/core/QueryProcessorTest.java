/*
 * Copyright (c) 2013-2024 Robert von Burg <eitch@eitchnet.ch>
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

import li.strolch.model.StrolchRootElement;
import org.junit.Assert;
import org.junit.Test;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class QueryProcessorTest extends BaseTest {

	/**
	 * test by string comparison of the result
	 */
	@Test
	public void testProcess() {

		List<StrolchRootElement> resources = getTestRessources(10);
		List<StrolchRootElement> orders = getTestOrders(10);

		Map<String, List<? extends StrolchRootElement>> inputCollections = new HashMap<>();
		inputCollections.put("r", resources);
		inputCollections.put("o", orders);

		QueryProcessor processor = new QueryProcessor();
		processor.setInputCollections(inputCollections);

		QueryRequest request = new QueryRequest();
		request.addParameter("id", "0");
		request.setStatement("SELECT r,o FROM Resource r, Order o WHERE r.getId() = :id AND o.getId() = :id");

		String expected =
				"{\"objectType\":\"QueryResponse\",\"statement\":\"SELECT r,o FROM Resource r, Order o WHERE r.getId() = :id AND "
						+ "o.getId() = :id\",\"queryParameter\":{\"id\":\"0\"},\"resultSet\":[[{\"objectType\":\"Resource\",\"id\":\"0\",\"name\":null,"
						+ "\"type\":null,\"parameterBags\":{\"testBag\":{\"id\":\"testBag\",\"name\":null,\"type\":null,"
						+ "\"parameters\":{\"testId\":{\"id\":\"testId\",\"name\":null,\"type\":\"Float\",\"value\":\"100.0\"}}}}},"
						+ "{\"objectType\":\"Order\",\"id\":\"0\",\"name\":null,\"type\":null,\"date\":\"2017-11-01T00:00:00+01:00\","
						+ "\"state\":\"Created\",\"parameterBags\":{\"testBag\":{\"id\":\"testBag\",\"name\":null,\"type\":null,"
						+ "\"parameters\":{\"testId\":{\"id\":\"testId\",\"name\":null,\"type\":\"Float\",\"value\":\"100.0\"}}}}}]]}";

		Assert.assertEquals(expected, processor.process(request, null).asJson().toString());
	}

	/**
	 * verify that an exception is thrown, if the select statement declares classes other than
	 * {@link StrolchRootElement}
	 */
	@Test
	public void testProcess2() {

		List<StrolchRootElement> resources = getTestRessources(10);
		List<StrolchRootElement> orders = getTestOrders(10);

		Map<String, List<? extends StrolchRootElement>> inputCollections = new HashMap<>();
		inputCollections.put("r", resources);
		inputCollections.put("o", orders);

		QueryProcessor processor = new QueryProcessor();
		processor.setInputCollections(inputCollections);

		QueryRequest request = new QueryRequest();
		request.addParameter("id", "5");
		request.setStatement(
				"SELECT r, o, r.getId() FROM Resource r, Order o WHERE r.getId() = :id AND o.getId() = :id");

		// TODO can be simpler but currently forgot how
		boolean thrown = false;
		try {
			System.out.println(processor.process(request, null).asJson().toString());
		} catch (Exception e) {
			thrown = true;
			Assert.assertTrue(e instanceof SOQLEvaluationException);
		}
		Assert.assertTrue(thrown);
	}

}
