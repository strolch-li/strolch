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

package li.strolch.soql.core.expresssion;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class SelectExpression extends AbstractObjectExpression {

	private final List<IObjectExpression> children = new ArrayList<>();

	/**
	 * @param inputObjects
	 * @param queryParameter
	 *
	 * @return List of objects from child expression evaluation
	 */
	@Override
	public List<Object> evaluate(final Map<String, Object> inputObjects, final Map<String, Object> queryParameter) {

		final List<Object> results = new ArrayList<>();
		for (IObjectExpression expr : children) {
			Object evaluate = expr.evaluate(inputObjects, queryParameter);
			if (evaluate != null)
				results.add(evaluate);
		}
		return results;
	}

	public void addExpression(final IObjectExpression expression) {
		children.add(expression);
		expression.setParent(this);
	}

	@Override
	public Class<?> getType(Map<String, Object> inputObjects, Map<String, Object> queryParameter) {
		return null;
	}

	@Override
	public String toString() {
		return "SelectExpression [children=" + children + "]";
	}

}
