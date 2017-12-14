package li.strolch.soql.core;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import li.strolch.model.Tags;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

/**
 * @author msmock
 */
public class QueryRequest {

    public static final String STATEMENT = "statement";
    public static final String PARAMETER = "parameter";

    // the SOQL query string
    private String statement;

    // the parameter of the SOQL query
    private Map<String, String> parameter = new HashMap<>();

    public String getStatement() {
        return statement;
    }

    public void setStatement(String statement) {
        this.statement = statement;
    }

    public Map<String, String> getParameter() {
        return parameter;
    }

    /**
     * @return the query as JsonObject
     */
    public JsonObject asJson() {

        final JsonObject rootJ = new JsonObject();
        rootJ.addProperty(Tags.Json.OBJECT_TYPE, "QueryRequest");
        rootJ.addProperty(STATEMENT, statement);

        final JsonObject parameterJ = new JsonObject();
        rootJ.add(PARAMETER, parameterJ);

        final Set<String> keys = parameter.keySet();
        for (Iterator<String> iterator = keys.iterator(); iterator.hasNext(); ) {
            final String key = iterator.next();
            final Object param = parameter.get(key);
            parameterJ.addProperty(key, param.toString());
        }

        return rootJ;
    }

    /**
     * build request from Json object
     *
     * @return
     */
    public QueryRequest fromJson(JsonObject jsonObject) {

        final String statement = jsonObject.get(STATEMENT).getAsString();
        setStatement(statement);

        final JsonObject params = jsonObject.getAsJsonObject(PARAMETER);
        final Set<Map.Entry<String, JsonElement>> entrySet = params.entrySet();
        for (final Map.Entry<String, JsonElement> entry : entrySet) {
            parameter.put(entry.getKey(), entry.getValue().getAsString());
        }

        return this;
    }

}
