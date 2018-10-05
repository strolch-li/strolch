package li.strolch.soql.core.expresssion;

import java.util.Map;

import li.strolch.soql.core.SOQLEvaluationException;

/**
 * A reference to a query parameter
 *
 * @author msmock
 */
public class ParameterReference extends AbstractObjectExpression {

	public String varName;

	@Override
	public Object evaluate(final Map<String, Object> inputObjects, final Map<String, Object> queryParameter) {
		Object result = queryParameter.get(varName);
		if (result == null)
			throw new SOQLEvaluationException("No object with key " + varName + " declared in the query parameter.");
		return result;
	}

	@Override
	public Class<?> getType(Map<String, Object> inputObjects, Map<String, Object> queryParameter) {
		return evaluate(inputObjects, queryParameter).getClass();
	}

	@Override
	public String toString() {
		return "VarReference [varName=" + varName + "]";
	}

}
