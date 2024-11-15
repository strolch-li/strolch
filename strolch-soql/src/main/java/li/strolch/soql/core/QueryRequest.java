/*
 * Copyright (c) 2013-2024 Robert von Burg <eitch@eitchnet.ch>
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

import com.google.gson.JsonObject;
import li.strolch.model.Tags;
import li.strolch.utils.dbc.DBC;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

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
