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

	// the parameter of the SOQL query
	private Map<String, Object> parameter;

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

	public void addParameter(String key, Object value) {
		if (this.parameter == null)
			this.parameter = new HashMap<>();
		this.parameter.put(key, value);
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

		Set<String> keys = this.parameter.keySet();
		for (String key : keys) {
			Object param = this.parameter.get(key);
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
