package li.strolch.soql.core.expresssion;

import java.util.Map;

/**
 * @author msmock
 */
public class WhereExpression extends AbstractBooleanExpression {

	private OrExpression orExpression;

	@Override
	public boolean evaluate(Map<String, Object> inputObjects, Map<String, Object> queryParameter) {
		return orExpression == null || orExpression.evaluate(inputObjects, queryParameter);
	}

	public void setOrExpression(OrExpression orExpression) {
		this.orExpression = orExpression;
		orExpression.setParent(this);
	}

	@Override
	public String toString() {
		return "WhereExpression [orExpression=" + orExpression + "]";
	}

}
