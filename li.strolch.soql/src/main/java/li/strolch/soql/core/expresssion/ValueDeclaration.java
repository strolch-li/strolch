package li.strolch.soql.core.expresssion;

import java.util.Map;

/**
 * Expression to set a fixed value
 * 
 * TODO allow blancs in Strings, and support parsing to other objects like int, double, Dates, etc. 
 * 
 * @author msmock
 */
public class ValueDeclaration extends AbstractObjectExpression {
	
	public String valueAsString;

	@Override
	public Object evaluate(final Map<String, Object> inputObjects, final Map<String, Object> queryParameter) {
		return valueAsString;
	}

	@Override
	public Class<?> getType(Map<String, Object> inputObjects, Map<String, Object> queryParameter) {
		return String.class;
	}

	@Override
	public String toString() {
		return "ValueDeclaration [valueAsString=" + valueAsString + "]";
	}
}
