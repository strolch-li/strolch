package li.strolch.soql.core;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import com.google.gson.JsonArray;

import li.strolch.model.StrolchRootElement;
import li.strolch.model.json.StrolchElementToJsonVisitor;

/**
 * The query result set as List of Lists
 * <p>
 *
 * TODO: the result set should carry arbitrary objects, not only StrolchRootElements
 *
 * @author msmock
 */
public class ResultSet {

	final StrolchElementToJsonVisitor visitor = new StrolchElementToJsonVisitor();

	public final List<List<StrolchRootElement>> rows = new ArrayList<>();

	/**
	 * @param row
	 *            the result of the execution of a single statement
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

		rows.add(toBeAdded);
	}

	/**
	 * @return all rows as JSON Array
	 */
	public JsonArray asJson() {

		JsonArray rowsAsJson = new JsonArray();

		for (Iterator<List<StrolchRootElement>> rowsIter = rows.iterator(); rowsIter.hasNext();)
			rowsAsJson.add(row2Json(rowsIter.next()));

		return rowsAsJson;
	}

	/**
	 * @return a single row as JSON Array
	 */
	private JsonArray row2Json(final List<StrolchRootElement> evalResult) {
		JsonArray rowAsJson = new JsonArray();
		for (Iterator<StrolchRootElement> iterator = evalResult.iterator(); iterator.hasNext();) {
			rowAsJson.add(iterator.next().accept(visitor));
		}
		return rowAsJson;
	}

}
