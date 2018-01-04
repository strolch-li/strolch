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
	public static final String PARAMETER = "queryParameter";

	// the SOQL query string
	String statement;

	// the parameter of the SOQL query
	Map<String, Object> parameter = new HashMap<>();

	public String getStatement() {
		return statement;
	}

	public void setStatement(String statement) {
		this.statement = statement;
	}

	public Map<String, Object> getParameter() {
		return parameter;
	}

	public void setParameter(Map<String, Object> parameter) {
		this.parameter = parameter;
	}

	/**
	 * @return the query as JsonObject
	 */
	public JsonObject asJson() {

		JsonObject rootJ = new JsonObject();
		rootJ.addProperty(Tags.Json.OBJECT_TYPE, "QueryRequest");
		rootJ.addProperty(STATEMENT, statement);

		JsonObject parameterJ = new JsonObject();
		rootJ.add(PARAMETER, parameterJ);

		Set<String> keys = parameter.keySet();
		for (Iterator<String> iterator = keys.iterator(); iterator.hasNext();) {
			String key = iterator.next();
			Object param = parameter.get(key);
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

		String statement = jsonObject.get(STATEMENT).getAsString();
		setStatement(statement);

		JsonObject params = jsonObject.getAsJsonObject(PARAMETER);
		Set<Map.Entry<String, JsonElement>> entrySet = params.entrySet();
		for (Map.Entry<String, JsonElement> entry : entrySet) {
			parameter.put(entry.getKey(), entry.getValue().getAsString());
		}

		return this;
	}

}
