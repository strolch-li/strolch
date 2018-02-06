package li.strolch.soql.core;

import static org.junit.Assert.assertEquals;

import java.util.List;

import org.junit.Test;

import com.google.gson.JsonObject;

/**
 * @author msmock
 */
public class SerialisationTest extends BaseTest {

	private QueryRequest buildTestRequest() {
		String s = "SELECT a FROM Activity a WHERE a.getId() = :p";
		final QueryRequest request = new QueryRequest();
		request.setStatement(s);
		request.getParameter().put("r", "Just a string!");
		return request;
	}

	private QueryResponse buildTestResponse() {
		String s = "SELECT a FROM Activity a WHERE a.getId() = :p";
		final QueryResponse response = new QueryResponse();
		response.setStatement(s);
		response.getParameter().put("r", "Just a string!");
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
		initial.getParameter().put("p", "10010");
		final JsonObject jsonObject = initial.asJson();

		final QueryRequest query = new QueryRequest();
		query.fromJson(jsonObject);

		assertEquals(s, query.getStatement());
		assertEquals("10010", query.getParameter().get("p"));
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	@Test
	public void testResponse2JSON() {
		final QueryResponse response = buildTestResponse();
		final List evalResult = getTestRessources(2);
		response.getResultSet().add(evalResult);

		String expected = "{\"objectType\":\"QueryResponse\",\"statement\":\"SELECT a FROM Activity a WHERE a.getId() = :p\",\"queryParameter\":{\"r\":\"Just a string!\"},\"resultSet\":[[{\"objectType\":\"Resource\",\"id\":\"0\",\"name\":null,\"type\":null,\"parameterBags\":{\"testBag\":{\"id\":\"testBag\",\"name\":null,\"type\":null,\"parameters\":{\"testId\":{\"id\":\"testId\",\"name\":null,\"type\":\"Float\",\"value\":\"100.0\"}}}}},{\"objectType\":\"Resource\",\"id\":\"1\",\"name\":null,\"type\":null,\"parameterBags\":{\"testBag\":{\"id\":\"testBag\",\"name\":null,\"type\":null,\"parameters\":{\"testId\":{\"id\":\"testId\",\"name\":null,\"type\":\"Float\",\"value\":\"100.0\"}}}}}]]}";

		final JsonObject jsonObject = response.asJson();
		assertEquals(expected.trim(), jsonObject.toString());

		System.out.println(jsonObject.toString());
	}

}
