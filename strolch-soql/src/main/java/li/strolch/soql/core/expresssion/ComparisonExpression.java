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

import li.strolch.soql.core.SOQLEvaluationException;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * Compares two or more expressions, for example 'a.name = "12345"'
 * <p>
 * Currently only the following operators for equality check are supported:
 * <p>
 * '=' | '<>' | '>' | '>=' | '<' | '<=' ;
 *
 * @author msmock
 */
public class ComparisonExpression extends AbstractBooleanExpression {

	private String operator;
	private final List<IObjectExpression> operands = new ArrayList<>();

	@Override
	public boolean evaluate(Map<String, Object> inputObjects, Map<String, Object> queryParameter) {

		return switch (operator) {
			case "=" -> evaluateEquals(inputObjects, queryParameter);
			case "<>" -> !evaluateEquals(inputObjects, queryParameter);
			case ">" -> evaluateMore(inputObjects, queryParameter);
			case "<" -> evaluateLess(inputObjects, queryParameter);
			case ">=" -> !evaluateLess(inputObjects, queryParameter);
			case "<=" -> !evaluateMore(inputObjects, queryParameter);
			default -> throw new SOQLEvaluationException(
					"Comparison with operator " + operator + " is not supported yet.");
		};
	}

	/**
	 * TODO: allow comparison, if the classes do not match. I.e. compare Integer with Double
	 *
	 * @param inputObjects
	 * @param queryParameter
	 *
	 * @return
	 */
	private boolean evaluateLess(final Map<String, Object> inputObjects, final Map<String, Object> queryParameter) {

		final Object result_1 = operands.get(0).evaluate(inputObjects, queryParameter);
		final Object result_2 = operands.get(1).evaluate(inputObjects, queryParameter);

		final Class<?> clazz_1 = result_1.getClass();
		final Class<?> clazz_2 = result_2.getClass();

		if (!clazz_1.equals(clazz_2)) {
			throw new SOQLEvaluationException("Operation < not defined for comparison of "
					+ result_1
					+ " of class "
					+ clazz_1
					+ " with "
					+ result_2
					+ " of class "
					+ clazz_2);
		}

		if (result_1 instanceof Integer && result_2 instanceof Integer) {
			return ((Integer) result_1).compareTo((Integer) result_2) < 0;
		} else if (result_1 instanceof Long && result_2 instanceof Long) {
			return ((Long) result_1).compareTo((Long) result_2) < 0;
		} else if (result_1 instanceof Float && result_2 instanceof Float) {
			return ((Float) result_1).compareTo((Float) result_2) < 0;
		} else if (result_1 instanceof Double && result_2 instanceof Double) {
			return ((Double) result_1).compareTo((Double) result_2) < 0;
		}

		return false;
	}

	private boolean evaluateMore(final Map<String, Object> inputObjects, final Map<String, Object> queryParameter) {

		final Object result_1 = operands.get(0).evaluate(inputObjects, queryParameter);
		final Object result_2 = operands.get(1).evaluate(inputObjects, queryParameter);

		final Class<?> clazz_1 = result_1.getClass();
		final Class<?> clazz_2 = result_2.getClass();

		if (!clazz_1.equals(clazz_2)) {
			throw new SOQLEvaluationException("Operation < not defined for comparison of "
					+ result_1
					+ " of class "
					+ clazz_1
					+ " with "
					+ result_2
					+ " of class "
					+ clazz_2);
		}

		if (result_1 instanceof Integer && result_2 instanceof Integer) {
			return ((Integer) result_1).compareTo((Integer) result_2) > 0;
		} else if (result_1 instanceof Long && result_2 instanceof Long) {
			return ((Long) result_1).compareTo((Long) result_2) > 0;
		} else if (result_1 instanceof Float && result_2 instanceof Float) {
			return ((Float) result_1).compareTo((Float) result_2) > 0;
		} else if (result_1 instanceof Double && result_2 instanceof Double) {
			return ((Double) result_1).compareTo((Double) result_2) > 0;
		}

		return false;
	}

	/**
	 * @return true if the String representation of the operands match
	 */
	private boolean evaluateEquals(final Map<String, Object> inputObjects, final Map<String, Object> queryParameter) {
		Object result_1 = operands.get(0).evaluate(inputObjects, queryParameter);
		Object result_2 = operands.get(1).evaluate(inputObjects, queryParameter);
		if (result_1 == null || result_2 == null)
			return false;
		return result_1.equals(result_2);
	}

	public void setOperator(String operator) {
		this.operator = operator;
	}

	public void addOperand(IObjectExpression operand) {
		operands.add(operand);
		operand.setParent(this);
	}

	@Override
	public String toString() {
		return "ComparisonExpression [operator=" + operator + ", operands=" + operands + "]";
	}

}
