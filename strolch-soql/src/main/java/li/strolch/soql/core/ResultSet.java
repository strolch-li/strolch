package li.strolch.soql.core;

import com.google.gson.JsonArray;
import li.strolch.model.StrolchRootElement;
import li.strolch.model.json.StrolchElementToJsonVisitor;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

/**
 * The query result set as List of Lists
 * <p>
 *
 * TODO: the result set should carry arbitrary objects, not only StrolchRootElements
 *
 * @author msmock
 */
public class ResultSet {

	private final List<List<StrolchRootElement>> rows = new ArrayList<>();

	/**
	 * @param row
	 * 		the result of the execution of a single statement
	 */
	public void add(final List<Object> row) {

		List<StrolchRootElement> toBeAdded = new LinkedList<>();
		for (Object object : row) {
			if (object instanceof StrolchRootElement) {
				toBeAdded.add((StrolchRootElement) object);
			} else {
				throw new SOQLEvaluationException("Could not add object " + object + " of class " + object.getClass()
						+ " to result set. Only StrolchRootElements are supported yet.");
			}
		}

		this.rows.add(toBeAdded);
	}

	/**
	 * @param flat
	 * 		if JSON should be flat or not
	 *
	 * @return all rows as JSON Array
	 */
	public JsonArray asJson(boolean flat) {

		JsonArray rowsAsJson = new JsonArray();

		StrolchElementToJsonVisitor visitor = new StrolchElementToJsonVisitor();
		if (flat)
			visitor.flat();

		for (List<StrolchRootElement> row : this.rows)
			rowsAsJson.add(row2Json(row, visitor));

		return rowsAsJson;
	}

	/**
	 * @return a single row as JSON Array
	 */
	private JsonArray row2Json(final List<StrolchRootElement> evalResult, StrolchElementToJsonVisitor visitor) {
		JsonArray rowAsJson = new JsonArray();
		for (StrolchRootElement anEvalResult : evalResult) {
			rowAsJson.add(anEvalResult.accept(visitor));
		}
		return rowAsJson;
	}

}
