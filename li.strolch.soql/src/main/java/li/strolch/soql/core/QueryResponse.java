package li.strolch.soql.core;

import com.google.gson.JsonObject;

import li.strolch.model.Tags;

/**
 * @author msmock
 */
public class QueryResponse extends QueryRequest {

	static final String RESULT_SET = "resultSet";

	// an exception or error message in case of error
	public String message;

	// the returned objects
	public ResultSet resultSet = new ResultSet();

	/**
	 * @param resultSet the resultSet to set
	 */
	public void setResultSet(ResultSet resultSet) {
		this.resultSet = resultSet;
	}

	/**
	 * @return the resultSet
	 */
	public ResultSet getResultSet() {
		return resultSet;
	}

	/**
	 * @return the query as JsonObject
	 */
	public JsonObject asJson() {

        JsonObject rootJ = super.asJson();
        rootJ.addProperty(Tags.Json.OBJECT_TYPE, "QueryResponse");

		if (message != null && !message.isEmpty()) {
			rootJ.addProperty("Message", message);
		}

		rootJ.add(RESULT_SET, resultSet.asJson());
		return rootJ;
	}

}
