package li.strolch.soql.core.expresssion;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class SelectClause implements IExpression {

	private IExpression parent;
	private List<IObjectExpression> selectExpressions = new ArrayList<>();

	/**
	 * @param inputObjects
	 * @param queryParameter
	 * 
	 * @return List of objects from child expression evaluation
	 */
	public List<Object> evaluate(final Map<String, Object> inputObjects, final Map<String, Object> queryParameter) {

		final List<Object> results = new ArrayList<>();
		for (IObjectExpression expr : selectExpressions) {
			Object evaluate = expr.evaluate(inputObjects, queryParameter);
			if (evaluate != null) {
				if (evaluate instanceof List<?>) {
					results.addAll((List<?>) evaluate);
				} else {
					results.add(evaluate);
				}
			}
		}
		return results;
	}

	public void addExpression(final IObjectExpression expression) {
		selectExpressions.add(expression);
		expression.setParent(this);
	}

	@Override
	public void setParent(final IExpression e) {
		this.parent = e;
	}

	@Override
	public IExpression getParent() {
		return parent;
	}

	@Override
	public String toString() {
		return "SelectClause [parent=" + parent + ", selectExpressions=" + selectExpressions + "]";
	}

}
