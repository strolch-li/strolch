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

	private List<ExpressionTerm> expressionTerms = new ArrayList<>();

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
