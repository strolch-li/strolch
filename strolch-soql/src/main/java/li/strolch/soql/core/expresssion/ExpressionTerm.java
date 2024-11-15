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

import java.util.Map;

/**
 * wraps a comparison with an optional boolean inversion
 *
 * @author msmock
 */
public class ExpressionTerm extends AbstractBooleanExpression {

	private boolean not = false;
	private ComparisonExpression comparisonExpression;

	@Override
	public boolean evaluate(final Map<String, Object> inputObjects, final Map<String, Object> queryParameter) {
		boolean result = comparisonExpression.evaluate(inputObjects, queryParameter);
		return not != result;
	}

	public void setNot(boolean not) {
		this.not = not;
	}

	public void setComparison(final ComparisonExpression comparison) {
		this.comparisonExpression = comparison;
		comparison.setParent(this);
	}

	@Override
	public String toString() {
		return "ExpressionTerm [not=" + not + ", comparison=" + comparisonExpression + "]";
	}

}
