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

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * Boolean AND implementation to combine {@link ComparisonExpression} objects
 *
 * @author msmock
 */
public class AndExpression extends AbstractBooleanExpression {

	private final List<ExpressionTerm> expressionTerms = new ArrayList<>();

	public void addExpressionTerm(ExpressionTerm e) {
		expressionTerms.add(e);
		e.setParent(this);
	}

	/**
	 * @return false if a child expression returns false
	 */
	@Override
	public boolean evaluate(Map<String, Object> inputObjects, Map<String, Object> queryParameter) {
		for (ExpressionTerm term : expressionTerms) {
			if (!term.evaluate(inputObjects, queryParameter))
				return false;
		}
		return true;
	}

	@Override
	public String toString() {
		return "AndExpression [expressionTerms=" + expressionTerms + "]";
	}

}
