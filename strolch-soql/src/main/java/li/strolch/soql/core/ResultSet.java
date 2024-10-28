/*
 * Copyright (c) 2024 Robert von Burg <eitch@eitchnet.ch>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

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
 * <p>
 * TODO: the result set should carry arbitrary objects, not only StrolchRootElements
 *
 * @author msmock
 */
public class ResultSet {

	private final List<List<StrolchRootElement>> rows = new ArrayList<>();

	/**
	 * @param row the result of the execution of a single statement
	 */
	public void add(final List<Object> row) {

		List<StrolchRootElement> toBeAdded = new LinkedList<>();
		for (Object object : row) {
			if (object instanceof StrolchRootElement) {
				toBeAdded.add((StrolchRootElement) object);
			} else {
				throw new SOQLEvaluationException("Could not add object "
						+ object
						+ " of class "
						+ object.getClass()
						+ " to result set. Only StrolchRootElements are supported yet.");
			}
		}

		this.rows.add(toBeAdded);
	}

	/**
	 * @param flat if JSON should be flat or not
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
