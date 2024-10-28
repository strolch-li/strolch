package li.strolch.soql.core;

import com.google.gson.JsonObject;
import li.strolch.model.Tags;

import static li.strolch.utils.helper.StringHelper.isNotEmpty;

/**
 * @author msmock
 */
public class QueryResponse extends QueryRequest {

	private static final String RESULT_SET = "resultSet";

	private String message;

	private ResultSet resultSet = new ResultSet();

	public String getMessage() {
		return this.message;
	}

	public void setMessage(String message) {
		this.message = message;
	}

	public void setResultSet(ResultSet resultSet) {
		this.resultSet = resultSet;
	}

	public ResultSet getResultSet() {
		return this.resultSet;
	}

	public JsonObject asJson() {
		return asJson(false);
	}

	public JsonObject asJson(boolean flat) {

		JsonObject rootJ = super.asJson();
		rootJ.addProperty(Tags.Json.OBJECT_TYPE, "QueryResponse");

		if (isNotEmpty(this.message)) {
			rootJ.addProperty("Message", this.message);
		}

		rootJ.add(RESULT_SET, this.resultSet.asJson(flat));
		return rootJ;
	}
}
