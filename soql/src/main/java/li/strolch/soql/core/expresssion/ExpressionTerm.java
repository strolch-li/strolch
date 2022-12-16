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
		return not ? !result : result;
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
