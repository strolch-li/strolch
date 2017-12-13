package li.strolch.soql.core;

import com.google.gson.JsonObject;
import li.strolch.model.StrolchRootElement;
import li.strolch.model.json.StrolchElementToJsonVisitor;
import org.junit.Test;

import java.util.ArrayList;
import java.util.List;

import static org.junit.Assert.assertEquals;

/**
 * @author msmock
 */
public class SerialisationTest extends BaseTest {

    final StrolchElementToJsonVisitor visitor = new StrolchElementToJsonVisitor();

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
    public void testSerialization() {
        final StrolchRootElement element = getTestResource("testId");
        final JsonObject jsonObject = element.accept(visitor);
        // System.out.println(jsonObject);

        String expected = "{\"objectType\":\"Resource\",\"id\":\"testId\",\"name\":null,\"type\":null,\"parameterBags\"" +
                ":{\"testBag\":{\"id\":\"testBag\",\"name\":null,\"type\":null,\"parameters\":{\"testId\":" +
                "{\"id\":\"testId\",\"name\":null,\"type\":\"Float\",\"value\":\"100.0\"}}}}}\n";

        assertEquals(expected.trim(), jsonObject.toString());
    }

    @Test
    public void testQuery2JSON() {
        final QueryRequest query = buildTestRequest();
        JsonObject jsonObject = query.toJson();

        String expected = "{\"objectType\":\"QueryRequest\",\"statement\":\"SELECT a FROM Activity a WHERE a.getId() " +
                "= :p\",\"parameter\":{\"r\":\"Just a string!\"}}";
        assertEquals(expected.trim(), jsonObject.toString());
    }

    @Test
    public void testJSON2Query() {

        String s = "SELECT a FROM Activity a WHERE a.getId() = :p";

        final QueryRequest initial = new QueryRequest();
        initial.setStatement(s);
        initial.getParameter().put("p", "10010");
        final JsonObject jsonObject = initial.toJson();

        final QueryRequest query = new QueryRequest();
        query.fromJson(jsonObject);

        assertEquals(s, query.getStatement());
        assertEquals("10010", query.getParameter().get("p"));
    }

    @Test
    public void testResponse2JSON() {
        final QueryResponse response = buildTestResponse();
        response.resultSet = getTestRessources(2);

        String expected = "{\"objectType\":\"QueryRequest\",\"statement\":\"SELECT a FROM Activity a WHERE a.getId() " +
                "= :p\",\"parameter\":{\"r\":\"Just a string!\"},\"resultSet\":[{\"objectType\":" +
                "\"Resource\",\"id\":\"2\",\"name\":null,\"type\":null,\"parameterBags\":" +
                "{\"testBag\":{\"id\":\"testBag\",\"name\":null,\"type\":null,\"parameters\":{\"testId\":{\"id\":" +
                "\"testId\",\"name\":null,\"type\":\"Float\",\"value\":\"100.0\"}}}}},{\"objectType\":\"Resource\"," +
                "\"id\":\"2\",\"name\":null,\"type\":null,\"parameterBags\":{\"testBag\":{\"id\":\"testBag\",\"name\"" +
                ":null,\"type\":null,\"parameters\":{\"testId\":{\"id\":\"testId\",\"name\":null,\"type\":\"Float\"," +
                "\"value\":\"100.0\"}}}}}]}\n";

        final JsonObject jsonObject = response.toJson(visitor);
        assertEquals(expected.trim(), jsonObject.toString());
    }

}
