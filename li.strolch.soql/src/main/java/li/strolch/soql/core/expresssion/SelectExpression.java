package li.strolch.soql.core.expresssion;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class SelectExpression extends AbstractObjectExpression {

	private List<IObjectExpression> children = new ArrayList<>();

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
