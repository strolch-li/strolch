package li.strolch.soql.core.expresssion;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * Boolean OR implementation to combine {@link ComparisonExpression} objects
 * 
 * @author msmock
 */
public class OrExpression extends AbstractBooleanExpression {

	private List<AndExpression> children = new ArrayList<>();

	/**
	 * @return true if at least one child expression returns true
	 */
	@Override
	public boolean evaluate(final Map<String, Object> inputObjects, final Map<String, Object> queryParameter) {
		for (AndExpression andExpression : children) {
			if (!andExpression.evaluate(inputObjects, queryParameter))
				return false;
		}
		return true;
	}

	public void addAndExpression(final AndExpression andExpression) {
		children.add(andExpression);
		andExpression.setParent(this);
	}

	@Override
	public String toString() {
		return "OrExpression [children=" + children + "]";
	}

}
