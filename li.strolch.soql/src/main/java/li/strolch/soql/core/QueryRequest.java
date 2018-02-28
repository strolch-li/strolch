package li.strolch.soql.core;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import com.google.gson.JsonObject;
import li.strolch.model.Tags;
import li.strolch.utils.dbc.DBC;

/**
 * @author msmock
 */
public class QueryRequest {

	public static final String STATEMENT = "statement";
	public static final String PARAMETER = "queryParameter";

	// the SOQL query string
	private String statement;

	// the parameterMap of the SOQL query
	private Map<String, Object> parameterMap;

	public String getStatement() {
		return statement;
	}

	public void setStatement(String statement) {
		this.statement = statement;
	}

	public Map<String, Object> getParameterMap() {
		return parameterMap;
	}

	public void setParameterMap(Map<String, Object> parameter) {
		this.parameterMap = parameter;
	}

	public void addParameter(String key, Object value) {
		if (this.parameterMap == null)
			this.parameterMap = new HashMap<>();
		this.parameterMap.put(key, value);
	}

	/**
	 * @return the query as JsonObject
	 */
	public JsonObject asJson() {

		JsonObject rootJ = new JsonObject();
		rootJ.addProperty(Tags.Json.OBJECT_TYPE, "QueryRequest");
		rootJ.addProperty(STATEMENT, this.statement);

		JsonObject parameterJ = new JsonObject();
		rootJ.add(PARAMETER, parameterJ);

		Set<String> keys = this.parameterMap.keySet();
		for (String key : keys) {
			Object param = this.parameterMap.get(key);
			parameterJ.addProperty(key, param.toString());
		}

		return rootJ;
	}

	/**
	 * build request from Json object
	 *
	 * @return the query request object
	 */
	public static QueryRequest fromJson(JsonObject jsonObject) {

		QueryRequest queryRequest = new QueryRequest();

		DBC.PRE.assertTrue("Expected json property " + STATEMENT, jsonObject.has(STATEMENT));
		String statement = jsonObject.get(STATEMENT).getAsString();
		queryRequest.setStatement(statement);

		if (jsonObject.has(PARAMETER)) {
			JsonObject params = jsonObject.getAsJsonObject(PARAMETER);
			Set<String> keys = params.keySet();
			for (String key : keys) {
				String value = params.get(key).getAsString();
				queryRequest.addParameter(key, value);
			}
		}

		return queryRequest;
	}
}
