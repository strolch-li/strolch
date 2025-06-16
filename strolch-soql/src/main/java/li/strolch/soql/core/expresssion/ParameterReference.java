/*
 * Copyright (c) 2013-2025 Robert von Burg <eitch@eitchnet.ch>
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

package li.strolch.soql.core.expresssion;

import li.strolch.soql.core.SOQLEvaluationException;

import java.util.Map;

/**
 * A reference to a query parameter
 *
 * @author msmock
 */
public class ParameterReference extends AbstractObjectExpression {

	public String varName;

	@Override
	public Object evaluate(final Map<String, Object> inputObjects, final Map<String, Object> queryParameter) {
		Object result = queryParameter.get(varName);
		if (result == null)
			throw new SOQLEvaluationException("No object with key " + varName + " declared in the query parameter.");
		return result;
	}

	@Override
	public Class<?> getType(Map<String, Object> inputObjects, Map<String, Object> queryParameter) {
		return evaluate(inputObjects, queryParameter).getClass();
	}

	@Override
	public String toString() {
		return "VarReference [varName=" + varName + "]";
	}

}
